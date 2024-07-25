(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Asttypes
open Types
module OCamlPath = Path

open Odoc_model.Paths
open Odoc_model.Lang
open Odoc_model.Names

module Env = Ident_env
module Paths = Odoc_model.Paths

module Compat = struct
#if OCAML_VERSION >= (4, 14, 0)
  (** this is the type on which physical equality is meaningful *)
  type repr_type_node = Types.transient_expr

   (** repr has morally type [type_expr -> repr_type_node] in all OCaml
       versions *)
  let repr x = Transient_expr.repr x

  let get_desc = Types.get_desc
  let get_row_name = Types.row_name
  let row_field_repr = Types.row_field_repr
  let field_kind_repr = Types.field_kind_repr
  let static_row_repr = Btype.static_row
  let row_closed = Types.row_closed
  let row_fields = Types.row_fields
  let field_public = Types.Fpublic
  let self_type = Btype.self_type
  let csig_self x = x.Types.csig_self
  let row_repr x = x
  let concr_mem = Types.Meths.mem
  let csig_concr x = x.Types.csig_meths
  let eq_type = Types.eq_type
  let invisible_wrap ty = newty2 ~level:Btype.generic_level (Ttuple [ty])
#else
  type repr_type_node = Types.type_expr
  let repr = Btype.repr
  let get_desc x = (repr x).Types.desc
  let get_row_name x = x.Types.row_name
  let row_field_repr = Btype.row_field_repr
  let field_kind_repr = Btype.field_kind_repr
  let static_row_repr x = Btype.static_row (Btype.row_repr x)
  let row_closed x = x.Types.row_closed
  let row_fields x = x.Types.row_fields
  let field_public = Types.Fpresent
  let self_type = Ctype.self_type
  let csig_self x = Btype.repr x.Types.csig_self
  let row_repr = Btype.row_repr
  let concr_mem = Types.Concr.mem
  let csig_concr x = x.Types.csig_concr
  let eq_type x y = x == y || repr x == repr y

  (** Create a new node pointing to [ty] that is printed in the same way as
      [ty]*)
  let invisible_wrap ty =
    Btype.(newty2 generic_level (Ttuple [ty]))
#endif
end

let proxy ty = Compat.(repr (Btype.proxy ty))

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let opt_iter f = function
  | None -> ()
  | Some x -> f x

let read_label lbl =
  let open TypeExpr in
#if OCAML_VERSION < (4,3,0)
  (* NOTE(@ostera): 4.02 does not have an Asttypes variant for whether the
   * label exists, and is an optional label or not, so I went back to string
   * manipulation *)
  if String.length lbl == 0
  then None
  else match String.get lbl 0 with
      | '?' -> Some (Optional (String.sub lbl 1 (String.length lbl - 1)))
      | _ -> Some (Label lbl)
#else
  match lbl with
  | Asttypes.Nolabel -> None
  | Asttypes.Labelled s -> Some (Label s)
  | Asttypes.Optional s -> Some (Optional s)
#endif

(* Handle type variable names *)

(** To identify equal type node for type variables, we need a map from the
    representative type node to names. Otherwise, equivalent variables would end
    up with distinct names *)
let used_names : (Compat.repr_type_node * string) list ref = ref []
let name_counter = ref 0
let reserved_names = ref []

let reset_names () = used_names := []; name_counter := 0; reserved_names := []

let reserve_name = function
  | Some name ->
      if not (List.mem name !reserved_names) then
        reserved_names := name :: !reserved_names
  | None -> ()

let rec next_name () =
  let name =
    if !name_counter < 26
    then String.make 1 (Char.chr(97 + !name_counter))
    else String.make 1 (Char.chr(97 + !name_counter mod 26)) ^
           string_of_int(!name_counter / 26)
  in
    incr name_counter;
    if List.mem name !reserved_names then next_name ()
    else name

let fresh_name base =
  let current_name = ref base in
  let i = ref 0 in
  while List.exists (fun (_, name') -> !current_name = name') !used_names do
    current_name := base ^ (string_of_int !i);
    i := !i + 1;
  done;
  !current_name

let name_of_type_repr (ty : Compat.repr_type_node) =
  try
    List.assq ty !used_names
  with Not_found ->
    let base =
      match ty.desc with
      | Tvar (Some name) | Tunivar (Some name) -> name
      | _ -> next_name ()
    in
    let name = fresh_name base in
    if name <> "_" then used_names := (ty, name) :: !used_names;
    name

let name_of_type ty = name_of_type_repr (Compat.repr ty)

let remove_names tyl =
  used_names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !used_names

(* Handle recursive types and shared row variables *)

let aliased: Compat.repr_type_node list ref = ref []
let used_aliases = ref []

let reset_aliased () = aliased := []; used_aliases := []

let is_aliased px = List.memq px !aliased

let aliasable (ty : Types.type_expr) =
  match Compat.get_desc ty with
  | Tvar _ | Tunivar _ | Tpoly _ -> false
  | _ -> true

let add_alias_proxy px =
  if not (List.memq px !aliased) then begin
    aliased := px :: !aliased;
    match px.desc with
    | Tvar name | Tunivar name -> reserve_name name
    | _ -> ()
  end

let add_alias ty = add_alias_proxy (proxy ty)

let used_alias (px : Compat.repr_type_node) = List.memq px !used_aliases

let use_alias (px : Compat.repr_type_node) = used_aliases := px :: !used_aliases

let visited_rows: Compat.repr_type_node list ref = ref []

let reset_visited_rows () = visited_rows := []

let is_row_visited px = List.memq px !visited_rows

let visit_row px =
  visited_rows := px :: !visited_rows

let visit_object ty px =
  if Ctype.opened_object ty then
    visited_rows := px :: !visited_rows

let namable_row row =
  Compat.get_row_name row <> None &&
  List.for_all
    (fun (_, f) ->
       match Compat.row_field_repr f with
#if OCAML_VERSION >= (4, 14, 0)
       | Reither(c, l, _) ->
#else
       | Reither(c, l, _, _) ->
#endif
           Compat.row_closed row && if c then l = [] else List.length l = 1
       | _ -> true)
    (Compat.row_fields row)

let mark_type ty =
  let rec loop visited ty =
    let px = proxy ty in
    if List.memq px visited && aliasable ty then add_alias_proxy px else
      let visited = px :: visited in
      match Compat.get_desc ty with
      | Tvar name -> reserve_name name
      | Tarrow(_, ty1, ty2, _) ->
          loop visited ty1;
          loop visited ty2
      | Ttuple tyl -> List.iter (loop visited) tyl
      | Tconstr(_, tyl, _) ->
          List.iter (loop visited) tyl
      | Tvariant row ->
          if is_row_visited px then add_alias_proxy px else
           begin
            if not (Compat.static_row_repr row) then visit_row px;
            match Compat.get_row_name row with
            | Some(_, tyl) when namable_row row ->
                List.iter (loop visited) tyl
            | _ ->
                Btype.iter_row (loop visited) row
           end
      | Tobject (fi, nm) ->
          if is_row_visited px then add_alias_proxy px else
           begin
            visit_object ty px;
            match !nm with
            | None ->
                let fields, _ = Ctype.flatten_fields fi in
                List.iter
                  (fun (_, kind, ty) ->
                    if Compat.field_kind_repr kind = Compat.field_public then
                      loop visited ty)
                  fields
            | Some (_, l) ->
                List.iter (loop visited) (List.tl l)
          end
      | Tfield(_, kind, ty1, ty2) when Compat.field_kind_repr kind = Compat.field_public ->
          loop visited ty1;
          loop visited ty2
      | Tfield(_, _, _, ty2) ->
          loop visited ty2
      | Tnil -> ()
      | Tpoly (ty, tyl) ->
          List.iter (fun t -> add_alias t) tyl;
          loop visited ty
      | Tunivar name -> reserve_name name
#if OCAML_VERSION>=(4,13,0)
      | Tpackage(_,tyl) ->
          List.iter (fun (_,x) -> loop visited x) tyl
#else
      | Tpackage(_, _, tyl) ->
          List.iter (loop visited) tyl
#endif
#if OCAML_VERSION<(4,13,0)
      | Tsubst ty -> loop visited ty
#else
      | Tsubst (ty,_) -> loop visited ty
#endif
      | Tlink _ -> assert false
  in
  loop [] ty

let reset_context () =
  reset_names ();
  reset_aliased ();
  reset_visited_rows ()

let mark_type_expr t =
  reset_context ();
  mark_type t

let mark_value_description vd =
  reset_context ();
  mark_type vd.val_type

let mark_type_parameter param =
  let px = proxy param in
  add_alias_proxy px;
  mark_type param;
  if aliasable param then use_alias px

#if OCAML_VERSION<(4,13,0)
let tvar_none ty = ty.desc <- Tvar None
#elif OCAML_VERSION < (4,14,0)
let tvar_none ty = Types.Private_type_expr.set_desc ty (Tvar None)
#else
let tvar_none ty = Types.Transient_expr.(set_desc (coerce ty) (Tvar None))
#endif

let wrap_constrained_params tyl =
  let params =
    List.fold_left
      (fun tyl ty ->
        if List.exists (Compat.eq_type ty) tyl
        then  Compat.invisible_wrap ty :: tyl
        else ty :: tyl)
      (* Two parameters might be identical due to a constraint but we need to
         print them differently in order to make the output syntactically valid.
         We use [Ttuple [ty]] because it is printed as [ty]. *)
      [] tyl
  in List.rev params

let prepare_type_parameters params manifest =
  let params = wrap_constrained_params params in
  begin match manifest with
    | Some ty ->
        let vars = Ctype.free_variables ty in
          List.iter
            (fun ty -> match Compat.get_desc ty with
              | Tvar (Some "_") -> if List.memq ty vars then tvar_none ty
              | _ -> ())
            params
    | None -> ()
  end;
  params

(* NOTE(@ostera): constructor with inlined records were introduced post 4.02 *)
let mark_constructor_args =
#if OCAML_VERSION < (4,3,0)
  List.iter mark_type
#else
  function
   | Cstr_tuple args -> List.iter mark_type args
   | Cstr_record lds -> List.iter (fun ld -> mark_type ld.ld_type) lds
#endif

let mark_type_kind = function
#if OCAML_VERSION >= (5,2,0)
  | Type_abstract _ -> ()
#else
  | Type_abstract -> ()
#endif
#if OCAML_VERSION >= (4,13,0)
  | Type_variant (cds,_) ->
#else
  | Type_variant cds ->
#endif
      List.iter
        (fun cd ->
           mark_constructor_args cd.cd_args;
           opt_iter mark_type cd.cd_res)
        cds
  | Type_record(lds, _) ->
      List.iter (fun ld -> mark_type ld.ld_type) lds
  | Type_open -> ()

let mark_type_declaration decl =
  let params = prepare_type_parameters decl.type_params decl.type_manifest in
    reset_context ();
    List.iter mark_type_parameter params;
    opt_iter mark_type decl.type_manifest;
    mark_type_kind decl.type_kind;
    params

let mark_extension_constructor ext =
  mark_constructor_args ext.ext_args;
  opt_iter mark_type ext.ext_ret_type

let mark_type_extension type_params exts =
  let type_params = prepare_type_parameters type_params None in
    reset_context ();
    List.iter mark_type_parameter type_params;
    List.iter mark_extension_constructor exts;
    type_params

let mark_type_extension' ext rest =
  let type_params = ext.ext_type_params in
  let exts = ext :: (List.map snd rest) in
    mark_type_extension type_params exts

let mark_exception ext =
  reset_context ();
  mark_extension_constructor ext

let rec mark_class_type params = function
  | Cty_constr (_, tyl, cty) ->
      let sty = Compat.self_type cty in
      if is_row_visited (proxy sty)
      || List.exists aliasable params
      || List.exists (Ctype.deep_occur sty) tyl
      then mark_class_type params cty
      else List.iter mark_type tyl
  | Cty_signature sign ->
      let sty = Compat.csig_self sign in
      let px = proxy sty in
      if is_row_visited px then add_alias_proxy px
      else visit_row px;
      let (fields, _) =
        Ctype.flatten_fields (Ctype.object_fields sign.csig_self)
      in
      List.iter (fun (_, _, ty) -> mark_type ty) fields;
      Vars.iter (fun _ (_, _, ty) -> mark_type ty) sign.csig_vars;
      if is_aliased px && aliasable sty then use_alias px
  | Cty_arrow (_, ty, cty) ->
      mark_type ty;
      mark_class_type params cty

let mark_class_type_declaration cltd =
  reset_context ();
  List.iter mark_type_parameter cltd.clty_params;
  mark_class_type cltd.clty_params cltd.clty_type

let mark_class_declaration cld =
  reset_context ();
  List.iter mark_type_parameter cld.cty_params;
  mark_class_type cld.cty_params cld.cty_type

let rec read_type_expr env typ =
  let open TypeExpr in
  let px = proxy typ in
  if used_alias px then Var (name_of_type typ)
  else begin
    let alias =
      if not (is_aliased px && aliasable typ) then None
      else begin
        use_alias px;
        Some (name_of_type typ)
      end
    in
    let typ =
      match Compat.get_desc typ with
      | Tvar _ ->
          let name = name_of_type typ in
            if name = "_" then Any
            else Var name
      | Tarrow(lbl, arg, res, _) ->
          let arg =
            if Btype.is_optional lbl then
              match Compat.get_desc arg with
              | Tconstr(_option, [arg], _) -> read_type_expr env arg
              | _ -> assert false
            else read_type_expr env arg
          in
          let lbl = read_label lbl in
          let res = read_type_expr env res in
            Arrow(lbl, arg, res)
      | Ttuple typs ->
          let typs = List.map (read_type_expr env) typs in
            Tuple typs
      | Tconstr(p, params, _) ->
          let p = Env.Path.read_type env p in
          let params = List.map (read_type_expr env) params in
            Constr(p, params)
      | Tvariant row -> read_row env px row
      | Tobject (fi, nm) -> read_object env fi !nm
      | Tnil | Tfield _ -> read_object env typ None
      | Tpoly (typ, []) -> read_type_expr env typ
      | Tpoly (typ, tyl) ->
          let tyl = List.map Compat.repr tyl in
          let vars = List.map name_of_type_repr tyl in
          let typ = read_type_expr env typ in
            remove_names tyl;
            Poly(vars, typ)
      | Tunivar _ -> Var (name_of_type typ)
#if OCAML_VERSION>=(4,13,0)
      | Tpackage(p,eqs) ->
#else
      | Tpackage(p, frags, tyl) ->
        let eqs = List.combine frags tyl in
#endif
          let open TypeExpr.Package in
          let path = Env.Path.read_module_type env p in
          let substitutions =
            List.map
              (fun (frag,typ) ->
                 let frag = Env.Fragment.read_type frag in
                 let typ = read_type_expr env typ in
                   (frag, typ))
              eqs
          in

          Package {path; substitutions}
#if OCAML_VERSION<(4,13,0)
      | Tsubst typ -> read_type_expr env typ
#else
      | Tsubst (typ,_) -> read_type_expr env typ
#endif
      | Tlink _ -> assert false
    in
      match alias with
      | None -> typ
      | Some name -> Alias(typ, name)
  end

and read_row env _px row =
  let open TypeExpr in
  let open TypeExpr.Polymorphic_variant in
  let row = Compat.row_repr row in
  let fields =
    if Compat.row_closed row then
      List.filter (fun (_, f) -> Compat.row_field_repr f <> Rabsent)
        (Compat.row_fields row)
    else Compat.row_fields row in
  let sorted_fields = List.sort (fun (p,_) (q,_) -> compare p q) fields in
  let present =
    List.filter
      (fun (_, f) ->
         match Compat.row_field_repr f with
         | Rpresent _ -> true
         | _ -> false)
      sorted_fields in
  let all_present = List.length present = List.length sorted_fields in
  match Compat.get_row_name row with
  | Some(p, params) when namable_row row ->
      let p = Env.Path.read_type env p in
      let params = List.map (read_type_expr env) params in
      if Compat.row_closed row && all_present then
        Constr (p, params)
      else
        let kind =
          if all_present then Open else Closed (List.map fst present)
        in
        Polymorphic_variant {kind; elements = [Type (Constr (p, params))]}
  | _ ->
      let elements =
        List.map
          (fun (name, f) ->
            match Compat.row_field_repr f with
              | Rpresent None ->
                Constructor {name; constant = true; arguments = []; doc = []}
              | Rpresent (Some typ) ->
                Constructor {
                  name;
                  constant = false;
                  arguments = [read_type_expr env typ];
                  doc = [];
                }
#if OCAML_VERSION >= (4, 14, 0)
              | Reither(constant, typs, _) ->
#else
              | Reither(constant, typs, _, _) ->
#endif
                let arguments =
                  List.map (read_type_expr env) typs
                in
                Constructor {name; constant; arguments; doc = []}
              | Rabsent -> assert false)
          sorted_fields
      in
      let kind =
        if all_present then
          if Compat.row_closed row then Fixed
          else Open
        else Closed (List.map fst present)
      in
      Polymorphic_variant {kind; elements}

and read_object env fi nm =
  let open TypeExpr in
  let open TypeExpr.Object in
  let px = proxy fi in
  if used_alias px then Var (name_of_type fi)
  else begin
    use_alias px;
    match nm with
    | None ->
        let (fields, rest) = Ctype.flatten_fields fi in
        let present_fields =
          List.fold_right
            (fun (n, k, t) l ->
               match Compat.field_kind_repr k with
               | f when f = Compat.field_public -> (n, t) :: l
               | _ -> l)
            fields []
        in
        let sorted_fields =
          List.sort (fun (n, _) (n', _) -> compare n n') present_fields
        in
        let methods =
          List.map
            (fun (name, typ) -> Method {name; type_ = read_type_expr env typ})
            sorted_fields
        in
        let open_ =
          match Compat.get_desc rest with
          | Tvar _ | Tunivar _ -> true
          | Tconstr _ -> true
          | Tnil -> false
          | _ -> assert false
        in
        Object {fields = methods; open_}
    | Some (p, _ :: params) ->
        let p = Env.Path.read_class_type env p in
        let params = List.map (read_type_expr env) params in
        Class (p, params)
    | _ -> assert false
  end

let read_value_description env parent id vd =
  let open Signature in
  let id = Env.find_value_identifier env id in
  let locs = None in
  let container =
    (parent : Identifier.Signature.t :> Identifier.LabelParent.t)
  in
  let doc = Doc_attr.attached_no_tag container vd.val_attributes in
  mark_value_description vd;
  let type_ = read_type_expr env vd.val_type in
  let value =
    match vd.val_kind with
    | Val_reg -> Value.Abstract
    | Val_prim desc ->
        let primitives =
          let open Primitive in
          desc.prim_name
          :: (match desc.prim_native_name with "" -> [] | name -> [ name ])
        in
        External primitives
    | _ -> assert false
  in
  Value { Value.id; locs; doc; type_; value }

let read_label_declaration env parent ld =
  let open TypeDecl.Field in
  let name = Ident.name ld.ld_id in
  let id = Identifier.Mk.field (parent, Odoc_model.Names.FieldName.make_std name) in
  let doc =
    Doc_attr.attached_no_tag
      (parent :> Identifier.LabelParent.t) ld.ld_attributes
  in
  let mutable_ = (ld.ld_mutable = Mutable) in
  let type_ = read_type_expr env ld.ld_type in
    {id; doc; mutable_; type_}

let read_constructor_declaration_arguments env parent arg =
#if OCAML_VERSION < (4,3,0)
  (* NOTE(@ostera): constructor with inlined records were introduced post 4.02
     so it's safe to use Tuple here *)
  ignore parent;
  TypeDecl.Constructor.Tuple(List.map (read_type_expr env) arg)
#else
  let open TypeDecl.Constructor in
    match arg with
    | Cstr_tuple args -> Tuple (List.map (read_type_expr env) args)
    | Cstr_record lds ->
        Record (List.map (read_label_declaration env parent) lds)
#endif

let read_constructor_declaration env parent cd =
  let open TypeDecl.Constructor in
  let id = Ident_env.find_constructor_identifier env cd.cd_id in
  let container = (parent :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container cd.cd_attributes in
  let args =
    read_constructor_declaration_arguments env
      (parent :> Identifier.FieldParent.t) cd.cd_args
  in
  let res = opt_map (read_type_expr env) cd.cd_res in
    {id; doc; args; res}

let read_type_kind env parent =
  let open TypeDecl.Representation in function
#if OCAML_VERSION >= (5,2,0)
  | Type_abstract _ ->
#else
  | Type_abstract ->
#endif
    None
#if OCAML_VERSION >= (4,13,0)
  | Type_variant (cstrs,_) ->
#else
  | Type_variant cstrs ->
#endif
        let cstrs =
          List.map (read_constructor_declaration env parent) cstrs
        in
          Some (Variant cstrs)
    | Type_record(lbls, _) ->
        let lbls =
          List.map
            (read_label_declaration env (parent :> Identifier.FieldParent.t))
            lbls
        in
          Some (Record lbls)
    | Type_open ->  Some Extensible

let read_injectivity var =
#if OCAML_VERSION < (5, 1, 0)
  let _, _, _, inj = Variance.get_lower var in
#else
  let _, _, inj = Variance.get_lower var in
#endif
  inj

let read_type_parameter abstr var param =
  let open TypeDecl in
  let name = name_of_type param in
  let desc =
    if name = "_" then Any
    else Var name
  in
  let variance =
    if not (abstr || aliasable param) then None
    else begin
      let co, cn = Variance.get_upper var in
        if not cn then Some Pos
        else if not co then Some Neg
        else None
      end in
  let injectivity = read_injectivity var in
  {desc; variance; injectivity}

let read_type_constraints env params =
  List.fold_right
    (fun typ1 acc ->
       let typ2 = Ctype.unalias typ1 in
       if Btype.proxy typ1 != Btype.proxy typ2 then
         let typ1 = read_type_expr env typ1 in
         let typ2 = read_type_expr env typ2 in
           (typ1, typ2) :: acc
       else acc)
    params []

let read_class_constraints env params =
  let open ClassSignature in
  read_type_constraints env params
  |> List.map (fun (left, right) ->
         Constraint { Constraint.left; right; doc = [] })

let read_type_declaration env parent id decl =
  let open TypeDecl in
  let id = Env.find_type_identifier env id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, canonical =
    Doc_attr.attached Odoc_model.Semantics.Expect_canonical container decl.type_attributes
  in
  let canonical = (canonical :> Path.Type.t option) in
  let params = mark_type_declaration decl in
  let manifest = opt_map (read_type_expr env) decl.type_manifest in
  let constraints = read_type_constraints env params in
  let representation = read_type_kind env (id :> Identifier.DataType.t) decl.type_kind in
  let abstr =
    match decl.type_kind with
#if OCAML_VERSION >= (5,2,0)
    | Type_abstract _ ->
#else
    | Type_abstract ->
#endif
        decl.type_manifest = None || decl.type_private = Private
    | Type_record _ ->
        decl.type_private = Private
#if OCAML_VERSION >= (4,13,0)
  | Type_variant (tll,_) ->
#else
  | Type_variant tll ->
#endif
        decl.type_private = Private ||
        List.exists (fun cd -> cd.cd_res <> None) tll
    | Type_open ->
        decl.type_manifest = None
  in
  let params =
    List.map2 (read_type_parameter abstr) decl.type_variance params
  in
  let private_ = (decl.type_private = Private) in
  let equation = Equation.{params; manifest; constraints; private_} in
  {id; locs; doc; canonical; equation; representation}

let read_extension_constructor env parent id ext =
  let open Extension.Constructor in
  let id = Env.find_extension_identifier env id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container ext.ext_attributes in
  let args =
    read_constructor_declaration_arguments env
      (parent : Identifier.Signature.t :> Identifier.FieldParent.t) ext.ext_args
  in
  let res = opt_map (read_type_expr env) ext.ext_ret_type in
  {id; locs; doc; args; res}

let read_type_extension env parent id ext rest =
  let open Extension in
  let type_path = Env.Path.read_type env ext.ext_type_path in
  let doc = Doc_attr.empty in
  let type_params = mark_type_extension' ext rest in
  let first = read_extension_constructor env parent id ext in
  let rest =
    List.map
      (fun (id, ext) -> read_extension_constructor env parent id ext)
      rest
  in
  let constructors = first :: rest in
  let type_params =
    List.map (read_type_parameter false Variance.null) type_params
  in
  let private_ = (ext.ext_private = Private) in
    { parent; type_path; type_params;
      doc; private_;
      constructors; }

let read_exception env parent id ext =
  let open Exception in
  let id = Env.find_exception_identifier env id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container ext.ext_attributes in
    mark_exception ext;
    let args =
      read_constructor_declaration_arguments env
        (parent : Identifier.Signature.t :> Identifier.FieldParent.t) ext.ext_args
    in
    let res = opt_map (read_type_expr env) ext.ext_ret_type in
    {id; locs; doc; args; res}

let read_method env parent concrete (name, kind, typ) =
  let open Method in
  let id = Identifier.Mk.method_(parent, Odoc_model.Names.MethodName.make_std name) in
  let doc = Doc_attr.empty in
  let private_ = (Compat.field_kind_repr kind) <> Compat.field_public in
  let virtual_ = not (Compat.concr_mem name concrete) in
  let type_ = read_type_expr env typ in
    ClassSignature.Method {id; doc; private_; virtual_; type_}

let read_instance_variable env parent (name, mutable_, virtual_, typ) =
  let open InstanceVariable in
  let id = Identifier.Mk.instance_variable(parent, Odoc_model.Names.InstanceVariableName.make_std name) in
  let doc = Doc_attr.empty in
  let mutable_ = (mutable_ = Mutable) in
  let virtual_ = (virtual_ = Virtual) in
  let type_ = read_type_expr env typ in
    ClassSignature.InstanceVariable {id; doc; mutable_; virtual_; type_}

let read_self_type sty =
  let px = proxy sty in
  if not (is_aliased px) then None
  else Some (TypeExpr.Var (name_of_type_repr px))

let rec read_class_signature env parent params =
  let open ClassType in function
  | Cty_constr(p, _, cty) ->
      if is_row_visited (proxy (Compat.self_type cty))
      || List.exists aliasable params
      then read_class_signature env parent params cty
      else begin
        let p = Env.Path.read_class_type env p in
        let params = List.map (read_type_expr env) params in
          Constr(p, params)
      end
  | Cty_signature csig ->
      let open ClassSignature in
      let self = read_self_type csig.csig_self in
      let constraints = read_class_constraints env params in
      let instance_variables =
        Vars.fold
          (fun name (mutable_, virtual_, typ) acc ->
             (name, mutable_, virtual_, typ) :: acc)
          csig.csig_vars []
      in
      let methods, _ =
        Ctype.flatten_fields (Ctype.object_fields csig.csig_self)
      in
      let methods =
        List.filter (fun (name, _, _) -> name <> Btype.dummy_method) methods
      in
      let instance_variables =
        List.map (read_instance_variable env parent) instance_variables
      in
      let methods =
        List.map (read_method env parent (Compat.csig_concr csig)) methods
      in
      let items = constraints @ instance_variables @ methods in
      Signature {self; items; doc = []}
  | Cty_arrow _ -> assert false

let rec read_virtual = function
  | Cty_constr(_, _, cty) | Cty_arrow(_, _, cty) -> read_virtual cty
  | Cty_signature csig ->
      let methods, _ =
        Ctype.flatten_fields (Ctype.object_fields csig.csig_self)
      in
      let virtual_method =
        List.exists
          (fun (name, _, _) ->
             not (name = Btype.dummy_method
                 || Compat.concr_mem name (Compat.csig_concr csig)))
          methods
      in
      let virtual_instance_variable =
        Vars.exists
          (fun _ (_, virtual_, _) -> virtual_ = Virtual)
          csig.csig_vars
      in
        virtual_method || virtual_instance_variable

let read_class_type_declaration env parent id cltd =
  let open ClassType in
  let id = Env.find_class_type_identifier env id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container cltd.clty_attributes in
    mark_class_type_declaration cltd;
    let params =
      List.map2
        (read_type_parameter false)
        cltd.clty_variance cltd.clty_params
    in
    let expr =
      read_class_signature env (id :> Identifier.ClassSignature.t) cltd.clty_params cltd.clty_type
    in
    let virtual_ = read_virtual cltd.clty_type in
    { id; locs; doc; virtual_; params; expr; expansion = None }

let rec read_class_type env parent params =
  let open Class in function
  | Cty_constr _ | Cty_signature _ as cty ->
      ClassType (read_class_signature env parent params cty)
  | Cty_arrow(lbl, arg, cty) ->
      let arg =
        if Btype.is_optional lbl then
          match Compat.get_desc arg with
          | Tconstr(path, [arg], _)
            when OCamlPath.same path Predef.path_option ->
              read_type_expr env arg
          | _ -> assert false
        else read_type_expr env arg
      in
      let lbl = read_label lbl in
      let cty = read_class_type env parent params cty in
        Arrow(lbl, arg, cty)

let read_class_declaration env parent id cld =
  let open Class in
  let id = Env.find_class_identifier env id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container cld.cty_attributes in
    mark_class_declaration cld;
    let params =
      List.map2
        (read_type_parameter false)
        cld.cty_variance cld.cty_params
    in
    let type_ =
      read_class_type env (id :> Identifier.ClassSignature.t) cld.cty_params cld.cty_type
    in
    let virtual_ = cld.cty_new = None in
    { id; locs; doc; virtual_; params; type_; expansion = None }

let rec read_module_type env parent (mty : Odoc_model.Compat.module_type) =
  let open ModuleType in
    match mty with
    | Mty_ident p -> Path {p_path = Env.Path.read_module_type env p; p_expansion=None }
    | Mty_signature sg -> Signature (read_signature env parent sg)
    | Mty_functor(parameter, res) ->
        let f_parameter, env =
          match parameter with
          | Unit -> Odoc_model.Lang.FunctorParameter.Unit, env
          | Named (id_opt, arg) ->
              let id, env = match id_opt with
                | None -> Identifier.Mk.parameter(parent, Odoc_model.Names.ModuleName.make_std "_"), env
                | Some id -> let env = Env.add_parameter parent id (ModuleName.of_ident id) env in
                  Ident_env.find_parameter_identifier env id, env
              in
              let arg = read_module_type env (id :> Identifier.Signature.t) arg in
              Odoc_model.Lang.FunctorParameter.Named ({ FunctorParameter. id; expr = arg }), env
        in
        let res = read_module_type env (Identifier.Mk.result parent) res in
        Functor( f_parameter, res)
    | Mty_alias p ->
        let t_desc = ModPath (Env.Path.read_module env p) in
        TypeOf { t_desc; t_expansion = None }

and read_module_type_declaration env parent id (mtd : Odoc_model.Compat.modtype_declaration) =
  let open ModuleType in
  let id = Env.find_module_type env id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, canonical = Doc_attr.attached Odoc_model.Semantics.Expect_canonical container mtd.mtd_attributes in
  let canonical = (canonical :> Path.ModuleType.t option) in
  let expr = opt_map (read_module_type env (id :> Identifier.Signature.t)) mtd.mtd_type in
  {id; locs; doc; canonical; expr }

and read_module_declaration env parent ident (md : Odoc_model.Compat.module_declaration) =
  let open Module in
  let id = (Env.find_module_identifier env ident :> Identifier.Module.t) in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, canonical = Doc_attr.attached Odoc_model.Semantics.Expect_canonical container md.md_attributes in
  let canonical = (canonical :> Path.Module.t option) in
  let type_ =
    match md.md_type with
    | Mty_alias p -> Alias (Env.Path.read_module env p, None)
    | _ -> ModuleType (read_module_type env (id :> Identifier.Signature.t) md.md_type)
  in
  let hidden =
    match canonical with
    | Some _ -> false
    | None -> Odoc_model.Root.contains_double_underscore (Ident.name ident)
  in
  {id; locs; doc; type_; canonical; hidden }

and read_type_rec_status rec_status =
  let open Signature in
  match rec_status with
  | Trec_first -> Ordinary
  | Trec_next -> And
  | Trec_not -> Nonrec

and read_module_rec_status rec_status =
  let open Signature in
  match rec_status with
  | Trec_not -> Ordinary
  | Trec_first -> Rec
  | Trec_next -> And

and read_signature_noenv env parent (items : Odoc_model.Compat.signature) =
  let rec loop (acc,shadowed) items =
    let open Signature in
    let open Odoc_model.Compat in
    let open Include in
    match items with
    | Sig_value(id, v, _) :: rest ->
        let vd = read_value_description env parent id v in
        let shadowed =
          if Env.is_shadowed env id
          then { shadowed with s_values = Odoc_model.Names.parenthesise (Ident.name id) :: shadowed.s_values }
          else shadowed
        in
          loop (vd :: acc, shadowed) rest
    | Sig_type(id, _, _, _) :: rest
        when Btype.is_row_name (Ident.name id) ->
        loop (acc, shadowed) rest
    | Sig_type(id, decl, rec_status, _)::rest ->
        let decl = read_type_declaration env parent id decl in
        let shadowed =
          if Env.is_shadowed env id
          then { shadowed with s_types = Ident.name id :: shadowed.s_types }
          else shadowed
        in
        loop (Type (read_type_rec_status rec_status, decl)::acc, shadowed) rest
    | Sig_typext (id, ext, Text_first, _) :: rest ->
        let rec inner_loop inner_acc = function
          | Sig_typext(id, ext, Text_next, _) :: rest ->
              inner_loop ((id, ext) :: inner_acc) rest
          | rest ->
              let ext =
                read_type_extension env parent id ext (List.rev inner_acc)
              in
                loop (TypExt ext :: acc, shadowed) rest
        in
          inner_loop [] rest
    | Sig_typext (id, ext, Text_next, _) :: rest ->
        let ext = read_type_extension env parent id ext [] in
          loop (TypExt ext :: acc, shadowed) rest
    | Sig_typext (id, ext, Text_exception, _) :: rest ->
        let exn = read_exception env parent id ext in
          loop (Exception exn :: acc, shadowed) rest
    | Sig_module (id, _, md, rec_status, _)::rest ->
          let md = read_module_declaration env parent id md in
          let shadowed =
            if Env.is_shadowed env id
            then { shadowed with s_modules = Ident.name id :: shadowed.s_modules }
            else shadowed
          in
            loop (Module (read_module_rec_status rec_status, md)::acc, shadowed) rest
    | Sig_modtype(id, mtd, _) :: rest ->
          let mtd = read_module_type_declaration env parent id mtd in
          let shadowed =
            if Env.is_shadowed env id
            then { shadowed with s_module_types = Ident.name id :: shadowed.s_module_types }
            else shadowed
          in
            loop (ModuleType mtd :: acc, shadowed) rest
#if OCAML_VERSION < (5,1,0)
    | Sig_class(id, cl, rec_status, _) :: Sig_class_type _
      :: Sig_type _ :: Sig_type _ :: rest ->
#else
    | Sig_class(id, cl, rec_status, _) :: Sig_class_type _
      :: Sig_type _ :: rest ->
#endif
          let cl = read_class_declaration env parent id cl in
          let shadowed =
            if Env.is_shadowed env id
            then { shadowed with s_classes = Ident.name id :: shadowed.s_classes }
            else shadowed
          in
            loop (Class (read_type_rec_status rec_status, cl)::acc, shadowed) rest
#if OCAML_VERSION < (5,1,0)
    | Sig_class_type(id, cltyp, rec_status, _)::Sig_type _::Sig_type _::rest ->
#else
    | Sig_class_type(id, cltyp, rec_status, _)::Sig_type _::rest ->
#endif
        let cltyp = read_class_type_declaration env parent id cltyp in
        let shadowed =
          if Env.is_shadowed env id
          then { shadowed with s_class_types = Ident.name id :: shadowed.s_class_types }
          else shadowed
        in
        loop (ClassType (read_type_rec_status rec_status, cltyp)::acc, shadowed) rest
    (* Skip all of the hidden sig items *)


  (* Bad - we expect Sig_class and Sig_class_type to be matched above
    with subsequent Sig_type items *)
    | Sig_class_type _ :: _
    | Sig_class _ :: _ -> assert false

    | [] -> ({items = List.rev acc; compiled=false; doc = [] }, shadowed)
  in
    loop ([],{s_modules=[]; s_module_types=[]; s_values=[];s_types=[]; s_classes=[]; s_class_types=[]}) items

and read_signature env parent (items : Odoc_model.Compat.signature) =
  let env = Env.handle_signature_type_items parent items env in
  fst @@ read_signature_noenv env parent items


let read_interface root name intf =
  let id = Identifier.Mk.root (root, Odoc_model.Names.ModuleName.make_std name) in
  let items = read_signature (Env.empty ()) id intf in
  (id, items)
