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
open Typedtree

module OCamlPath = Path

open Odoc_model.Paths
open Odoc_model.Lang
open Odoc_model.Names

module Env = Ident_env
module Paths = Odoc_model.Paths

let read_module_expr : (Ident_env.t -> Identifier.Signature.t -> Identifier.LabelParent.t -> Typedtree.module_expr -> ModuleType.expr) ref = ref (fun _ _ _ _ -> failwith "unset")

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let read_label = Cmi.read_label

let rec read_core_type env container ctyp =
  let open TypeExpr in
    match ctyp.ctyp_desc with
    | Ttyp_any -> Any
    | Ttyp_var s -> Var s
    | Ttyp_arrow(lbl, arg, res) ->
        let lbl = read_label lbl in
#if OCAML_VERSION < (4,3,0)
        (* NOTE(@ostera): Unbox the optional value for this optional labelled
           argument since the 4.02.x representation includes it explicitly. *)
        let arg = match lbl with
          | None | Some(Label(_)) -> read_core_type env container arg
          | Some(Optional(_)) ->
              let arg' = match arg.ctyp_desc with
                | Ttyp_constr(_, _, param :: _) -> param
                | _ -> arg
              in
              read_core_type env container arg'
#else
        let arg = read_core_type env container arg
#endif
        in
        let res = read_core_type env container res in
          Arrow(lbl, arg, res)
    | Ttyp_tuple typs ->
        let typs = List.map (read_core_type env container) typs in
          Tuple typs
    | Ttyp_constr(p, _, params) ->
        let p = Env.Path.read_type env p in
        let params = List.map (read_core_type env container) params in
          Constr(p, params)
    | Ttyp_object(methods, closed) ->
        let open TypeExpr.Object in
        let fields =
          List.map
#if OCAML_VERSION < (4,6,0)
            (fun (name, _, typ) ->
              Method {name; type_ = read_core_type env container typ})
#elif OCAML_VERSION < (4,8,0)
            (function
              | OTtag (name, _, typ) ->
                Method {
                  name = name.txt;
                  type_ = read_core_type env container typ;
                }
              | OTinherit typ -> Inherit (read_core_type env container typ))
#else
            (function
              | {of_desc=OTtag (name, typ); _} ->
                Method {
                  name = name.txt;
                  type_ = read_core_type env container typ;
                }
              | {of_desc=OTinherit typ; _} -> Inherit (read_core_type env container typ))
#endif
            methods
        in
          Object {fields; open_ = (closed = Asttypes.Open)}
    | Ttyp_class(p, _, params) ->
        let p = Env.Path.read_class_type env p in
        let params = List.map (read_core_type env container) params in
          Class(p, params)
    | Ttyp_alias(typ, var) ->
        let typ = read_core_type env container typ in
#if OCAML_VERSION >= (5,2,0)
        Alias(typ, var.txt)
#else
        Alias(typ, var)
#endif
    | Ttyp_variant(fields, closed, present) ->
        let open TypeExpr.Polymorphic_variant in
        let elements =
          fields |> List.map begin fun field ->
#if OCAML_VERSION >= (4,8,0)
            match field.rf_desc with
              | Ttag(name, constant, arguments) ->
                let attributes = field.rf_attributes in
#else
            match field with
              | Ttag(name, attributes, constant, arguments) ->
#endif
                let arguments =
                  List.map (read_core_type env container) arguments in
#if OCAML_VERSION >= (4,6,0)
                  let name = name.txt in
#endif
                let doc = Doc_attr.attached_no_tag container attributes in
                Constructor {name; constant; arguments; doc}
              | Tinherit typ -> Type (read_core_type env container typ)
          end
        in
        let kind =
          if closed = Asttypes.Open then Open
          else match present with
            | None -> Fixed
            | Some names -> Closed names
        in
          Polymorphic_variant {kind; elements}
    | Ttyp_poly([], typ) -> read_core_type env container typ
    | Ttyp_poly(vars, typ) -> Poly(vars, read_core_type env container typ)
    | Ttyp_package {pack_path; pack_fields; _} ->
        let open TypeExpr.Package in
        let path = Env.Path.read_module_type env pack_path in
        let substitutions =
          List.map
            (fun (frag, typ) ->
               let frag = Env.Fragment.read_type frag.Location.txt in
               let typ = read_core_type env container typ in
               (frag, typ))
            pack_fields
        in
          Package {path; substitutions}
#if OCAML_VERSION >= (5,2,0)
    | Ttyp_open (_p,_l,t) ->
      (* TODO: adjust model *)
      read_core_type env container t
#endif

let read_value_description env parent vd =
  let open Signature in
  let id = Env.find_value_identifier env vd.val_id in
  let locs = None in
  let container =
    (parent : Identifier.Signature.t :> Identifier.LabelParent.t)
  in
  let doc = Doc_attr.attached_no_tag container vd.val_attributes in
  let type_ = read_core_type env container vd.val_desc in
  let value =
    match vd.val_prim with
    | [] -> Value.Abstract
    | primitives -> External primitives
  in
  Value { Value.id; locs; doc; type_; value }

let read_type_parameter (ctyp, var_and_injectivity)  =
  let open TypeDecl in
  let desc =
    match ctyp.ctyp_desc with
    | Ttyp_any -> Any
    | Ttyp_var s -> Var s
    | _ -> assert false
  in
  let variance, injectivity =
#if OCAML_VERSION < (4,12,0)
    let var =
      match var_and_injectivity with
      | Covariant -> Some Pos
      | Contravariant -> Some Neg
      | Invariant -> None in
        var, false
#else
    let var =
      match fst var_and_injectivity with
      | Covariant -> Some Pos
      | Contravariant -> Some Neg
      | NoVariance -> None in
    let injectivity = match snd var_and_injectivity with
      | Injective -> true
      | NoInjectivity -> false in
    var, injectivity
#endif
  in
    {desc; variance; injectivity}

let read_label_declaration env parent label_parent ld =
  let open TypeDecl.Field in
  let open Odoc_model.Names in
  let name = Ident.name ld.ld_id in
  let id = Identifier.Mk.field(parent, FieldName.make_std name) in
  let doc = Doc_attr.attached_no_tag label_parent ld.ld_attributes in
  let mutable_ = (ld.ld_mutable = Mutable) in
  let type_ = read_core_type env label_parent ld.ld_type in
    {id; doc; mutable_; type_}

let read_constructor_declaration_arguments env parent label_parent arg =
  let open TypeDecl.Constructor in
#if OCAML_VERSION < (4,3,0)
  ignore parent;
  Tuple (List.map (read_core_type env label_parent) arg)
#else
  match arg with
  | Cstr_tuple args -> Tuple (List.map (read_core_type env label_parent) args)
  | Cstr_record lds ->
      Record (List.map (read_label_declaration env parent label_parent) lds)
#endif

let read_constructor_declaration env parent cd =
  let open TypeDecl.Constructor in
  let id = Ident_env.find_constructor_identifier env cd.cd_id in
  let container = (parent :> Identifier.FieldParent.t) in
  let label_container = (container :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag label_container cd.cd_attributes in
  let args =
    read_constructor_declaration_arguments
      env container label_container cd.cd_args
  in
  let res = opt_map (read_core_type env label_container) cd.cd_res in
    {id; doc; args; res}

let read_type_kind env parent =
  let open TypeDecl.Representation in function
    | Ttype_abstract -> None
    | Ttype_variant cstrs ->
        let cstrs = List.map (read_constructor_declaration env parent) cstrs in
          Some (Variant cstrs)
    | Ttype_record lbls ->
      let parent = (parent :> Identifier.FieldParent.t) in
      let label_parent = (parent :> Identifier.LabelParent.t) in
      let lbls =
        List.map (read_label_declaration env parent label_parent) lbls in
          Some (Record lbls)
    | Ttype_open -> Some Extensible

let read_type_equation env container decl =
  let open TypeDecl.Equation in
  let params = List.map read_type_parameter decl.typ_params in
  let private_ = (decl.typ_private = Private) in
  let manifest = opt_map (read_core_type env container) decl.typ_manifest in
  let constraints =
    List.map
      (fun (typ1, typ2, _) ->
         (read_core_type env container typ1,
          read_core_type env container typ2))
      decl.typ_cstrs
  in
    {params; private_; manifest; constraints}

let read_type_declaration env parent decl =
  let open TypeDecl in
  let id = Env.find_type_identifier env decl.typ_id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, canonical = Doc_attr.attached Odoc_model.Semantics.Expect_canonical container decl.typ_attributes in
  let canonical = (canonical :> Path.Type.t option) in
  let equation = read_type_equation env container decl in
  let representation = read_type_kind env id decl.typ_kind in
  {id; locs; doc; canonical; equation; representation}

let read_type_declarations env parent rec_flag decls =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let items =
    let open Signature in
    List.fold_left
      (fun (acc, recursive) decl ->
        if Btype.is_row_name (Ident.name decl.typ_id)
        then (acc, recursive)
        else begin
          let comments =
            Doc_attr.standalone_multiple container decl.typ_attributes in
           let comments = List.map (fun com -> Comment com) comments in
           let decl = read_type_declaration env parent decl in
           ((Type (recursive, decl)) :: (List.rev_append comments acc), And)
        end)
      ([], rec_flag) decls
    |> fst
  in
    List.rev items

#if OCAML_VERSION >= (4,8,0)
let read_type_substitutions env parent decls =
  List.map (fun decl -> Odoc_model.Lang.Signature.TypeSubstitution (read_type_declaration env parent decl)) decls
#endif

let read_extension_constructor env parent ext =
  let open Extension.Constructor in
  let id = Env.find_extension_identifier env ext.ext_id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.FieldParent.t) in
  let label_container = (container :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag label_container ext.ext_attributes in
  match ext.ext_kind with
  | Text_rebind _ -> assert false
#if OCAML_VERSION >= (4, 14, 0)
  | Text_decl(_, args, res) ->
#else
  | Text_decl(args, res) ->
#endif
    let args =
      read_constructor_declaration_arguments
        env container label_container args
    in
    let res = opt_map (read_core_type env label_container) res in
    {id; locs; doc; args; res}

let read_type_extension env parent tyext =
  let open Extension in
  let type_path = Env.Path.read_type env tyext.tyext_path in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container tyext.tyext_attributes in
  let type_params = List.map read_type_parameter tyext.tyext_params in
  let private_ = (tyext.tyext_private = Private) in
  let constructors =
    List.map (read_extension_constructor env parent) tyext.tyext_constructors
  in
    { parent; type_path; doc; type_params; private_; constructors; }

let read_exception env parent (ext : extension_constructor) =
  let open Exception in
  let id = Env.find_exception_identifier env ext.ext_id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.FieldParent.t) in
  let label_container = (container :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag label_container ext.ext_attributes in
  match ext.ext_kind with
  | Text_rebind _ -> assert false
#if OCAML_VERSION >= (4, 14, 0)
  | Text_decl(_, args, res) ->
#else
  | Text_decl(args, res) ->
#endif
    let args =
      read_constructor_declaration_arguments
        env container label_container args
    in
    let res = opt_map (read_core_type env label_container) res in
    {id; locs; doc; args; res}

let rec read_class_type_field env parent ctf =
  let open ClassSignature in
  let open Odoc_model.Names in
  let container = (parent : Identifier.ClassSignature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container ctf.ctf_attributes in
  match ctf.ctf_desc with
  | Tctf_val(name, mutable_, virtual_, typ) ->
      let open InstanceVariable in
      let id = Identifier.Mk.instance_variable(parent, InstanceVariableName.make_std name) in
      let mutable_ = (mutable_ = Mutable) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env container typ in
      Some (InstanceVariable {id; doc; mutable_; virtual_; type_})
  | Tctf_method(name, private_, virtual_, typ) ->
      let open Method in
      let id = Identifier.Mk.method_(parent, MethodName.make_std name) in
      let private_ = (private_ = Private) in
      let virtual_ = (virtual_ = Virtual) in
      let type_ = read_core_type env container typ in
      Some (Method {id; doc; private_; virtual_; type_})
  | Tctf_constraint(typ1, typ2) ->
      let left = read_core_type env container typ1 in
      let right = read_core_type env container typ2 in
      Some (Constraint {left; right; doc})
  | Tctf_inherit cltyp ->
      let expr = read_class_signature env parent container cltyp in
      Some (Inherit {expr; doc})
  | Tctf_attribute attr ->
      match Doc_attr.standalone container attr with
      | None -> None
      | Some doc -> Some (Comment doc)

and read_self_type env container typ =
  if typ.ctyp_desc = Ttyp_any then None
  else Some (read_core_type env container typ)

and read_class_signature env parent label_parent cltyp =
  let open ClassType in
    match cltyp.cltyp_desc with
    | Tcty_constr(p, _, params) ->
        let p = Env.Path.read_class_type env p in
      let params = List.map (read_core_type env label_parent) params in
          Constr(p, params)
    | Tcty_signature csig ->
        let open ClassSignature in
      let self = read_self_type env label_parent csig.csig_self in
        let items =
          List.fold_left
            (fun rest item ->
               match read_class_type_field env parent item with
               | None -> rest
               | Some item -> item :: rest)
            [] csig.csig_fields
        in
        let items = List.rev items in
        let items, (doc, doc_post) = Doc_attr.extract_top_comment_class items in
        let items =
          match doc_post with
          | [] -> items
          | _ -> Comment (`Docs doc_post) :: items
        in
        Signature {self; items; doc}
    | Tcty_arrow _ -> assert false
#if OCAML_VERSION >= (4,8,0)
  | Tcty_open (_, cty) -> read_class_signature env parent label_parent cty
#elif OCAML_VERSION >= (4,6,0)
  | Tcty_open (_, _, _, _, cty) -> read_class_signature env parent label_parent cty
#endif

let read_class_type_declaration env parent cltd =
  let open ClassType in
  let id = Env.find_class_type_identifier env cltd.ci_id_class_type in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container cltd.ci_attributes in
  let virtual_ = (cltd.ci_virt = Virtual) in
  let params = List.map read_type_parameter cltd.ci_params in
  let expr = read_class_signature env (id :> Identifier.ClassSignature.t) container cltd.ci_expr in
  { id; locs; doc; virtual_; params; expr; expansion = None }

let read_class_type_declarations env parent cltds =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let open Signature in
  List.fold_left begin fun (acc,recursive) cltd ->
    let comments = Doc_attr.standalone_multiple container cltd.ci_attributes in
    let comments = List.map (fun com -> Comment com) comments in
    let cltd = read_class_type_declaration env parent cltd in
    ((ClassType (recursive, cltd))::(List.rev_append comments acc), And)
  end ([], Ordinary) cltds
  |> fst
  |> List.rev

let rec read_class_type env parent label_parent cty =
  let open Class in
  match cty.cltyp_desc with
  | Tcty_constr _ | Tcty_signature _ ->
    ClassType (read_class_signature env parent label_parent cty)
  | Tcty_arrow(lbl, arg, res) ->
      let lbl = read_label lbl in
    let arg = read_core_type env label_parent arg in
    let res = read_class_type env parent label_parent res in
        Arrow(lbl, arg, res)
#if OCAML_VERSION >= (4,8,0)
  | Tcty_open (_, cty) -> read_class_type env parent label_parent cty
#elif OCAML_VERSION >= (4,6,0)
  | Tcty_open (_, _, _, _, cty) -> read_class_type env parent label_parent cty
#endif

let read_class_description env parent cld =
  let open Class in
  let id = Env.find_class_identifier env cld.ci_id_class in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container cld.ci_attributes in
  let virtual_ = (cld.ci_virt = Virtual) in
  let params = List.map read_type_parameter cld.ci_params in
  let type_ = read_class_type env (id :> Identifier.ClassSignature.t) container cld.ci_expr in
  { id; locs; doc; virtual_; params; type_; expansion = None }

let read_class_descriptions env parent clds =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let open Signature in
  List.fold_left begin fun (acc, recursive) cld ->
    let comments = Doc_attr.standalone_multiple container cld.ci_attributes in
    let comments = List.map (fun com -> Comment com) comments in
    let cld = read_class_description env parent cld in
    ((Class (recursive, cld))::(List.rev_append comments acc), And)
  end ([], Ordinary) clds
  |> fst
  |> List.rev

let rec read_with_constraint env global_parent parent (_, frag, constr) =
  let _ = global_parent in
  let open ModuleType in
    match constr with
    | Twith_type decl ->
        let frag = Env.Fragment.read_type frag.Location.txt in
        let eq = read_type_equation env parent decl in
          TypeEq(frag, eq)
    | Twith_module(p, _) ->
        let frag = Env.Fragment.read_module frag.Location.txt in
        let eq = read_module_equation env p in
          ModuleEq(frag, eq)
    | Twith_typesubst decl ->
        let frag = Env.Fragment.read_type frag.Location.txt in
        let eq = read_type_equation env parent decl in
          TypeSubst(frag, eq)
    | Twith_modsubst(p, _) ->
        let frag = Env.Fragment.read_module frag.Location.txt in
        let p = Env.Path.read_module env p in
          ModuleSubst(frag, p)
#if OCAML_VERSION >= (4,13,0)
    | Twith_modtype mty ->
        let frag = Env.Fragment.read_module_type frag.Location.txt in
        let mty = read_module_type env global_parent parent mty in
        ModuleTypeEq(frag, mty)
    | Twith_modtypesubst mty ->
        let frag = Env.Fragment.read_module_type frag.Location.txt in
        let mty = read_module_type env global_parent parent mty in
        ModuleTypeSubst(frag, mty)
#endif

and read_module_type env parent label_parent mty =
  let open ModuleType in
    match mty.mty_desc with
    | Tmty_ident(p, _) -> Path { p_path = Env.Path.read_module_type env p; p_expansion = None }
    | Tmty_signature sg ->
        let sg, () = read_signature Odoc_model.Semantics.Expect_none env parent sg in
        Signature sg
#if OCAML_VERSION >= (4,10,0)
    | Tmty_functor(parameter, res) ->
        let f_parameter, env =
          match parameter with
          | Unit -> FunctorParameter.Unit, env
          | Named (id_opt, _, arg) ->
            let id, env =
              match id_opt with
              | None -> Identifier.Mk.parameter (parent, ModuleName.make_std "_"), env
              | Some id ->
                 let env = Env.add_parameter parent id (ModuleName.of_ident id) env in
                 Env.find_parameter_identifier env id, env
            in
            let arg = read_module_type env (id :> Identifier.Signature.t) label_parent arg in
            Named { id; expr = arg; }, env
        in
        let res = read_module_type env (Identifier.Mk.result parent) label_parent res in
        Functor (f_parameter, res)
#else
    | Tmty_functor(id, _, arg, res) ->
        let new_env = Env.add_parameter parent id (ModuleName.of_ident id) env in
        let f_parameter =
          match arg with
          | None -> Odoc_model.Lang.FunctorParameter.Unit
          | Some arg ->
              let id = Ident_env.find_parameter_identifier new_env id in
              let arg = read_module_type env (id :> Identifier.Signature.t) label_parent arg in
              Named { FunctorParameter. id; expr = arg }
        in
        let res = read_module_type new_env (Identifier.Mk.result parent) label_parent res in
        Functor( f_parameter, res)
#endif
    | Tmty_with(body, subs) -> (
      let body = read_module_type env parent label_parent body in
      let subs = List.map (read_with_constraint env parent label_parent) subs in
      match Odoc_model.Lang.umty_of_mty body with
      | Some w_expr ->
          With {w_substitutions=subs; w_expansion=None; w_expr }
      | None ->
        failwith "error")
    | Tmty_typeof mexpr ->
        let decl =
          match mexpr.mod_desc with
          | Tmod_ident(p, _) ->
            TypeOf {t_desc = ModPath (Env.Path.read_module env p); t_expansion=None}
          | Tmod_structure {str_items = [{str_desc = Tstr_include {incl_mod; _}; _}]; _} -> begin
            match Typemod.path_of_module incl_mod with
            | Some p -> TypeOf {t_desc=StructInclude (Env.Path.read_module env p); t_expansion=None}
            | None ->
              !read_module_expr env parent label_parent mexpr 
            end
          | _  ->
            !read_module_expr env parent label_parent mexpr 
          in
        decl
    | Tmty_alias _ -> assert false

(** Like [read_module_type] but handle the canonical tag in the top-comment. If
    [canonical] is [Some _], no tag is expected in the top-comment. *)
and read_module_type_maybe_canonical env parent container ~canonical mty =
  match (canonical, mty.mty_desc) with
  | None, Tmty_signature sg ->
      let sg, canonical =
        read_signature Odoc_model.Semantics.Expect_canonical env parent sg
      in
      (ModuleType.Signature sg, canonical)
  | _, _ -> (read_module_type env parent container mty, canonical)

and read_module_type_declaration env parent mtd =
  let open ModuleType in
  let id = Env.find_module_type env mtd.mtd_id in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, canonical = Doc_attr.attached Odoc_model.Semantics.Expect_canonical container mtd.mtd_attributes in
  let expr, canonical =
    match mtd.mtd_type with
    | Some mty ->
        let expr, canonical =
          read_module_type_maybe_canonical env
            (id :> Identifier.Signature.t)
            container ~canonical mty
        in
        (Some expr, canonical)
    | None -> (None, canonical)
  in
  let canonical = (canonical :> Path.ModuleType.t option) in
  { id; locs; doc; canonical; expr }

and read_module_declaration env parent md =
  let open Module in
#if OCAML_VERSION >= (4,10,0)
  match md.md_id with
  | None -> None
  | Some id ->
  let id = Env.find_module_identifier env id in
#else
  let id = Env.find_module_identifier env md.md_id in
#endif
  let id = (id :> Identifier.Module.t) in
  let locs = None in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, canonical = Doc_attr.attached Odoc_model.Semantics.Expect_canonical container md.md_attributes in
  let type_, canonical =
    match md.md_type.mty_desc with
    | Tmty_alias (p, _) -> (Alias (Env.Path.read_module env p, None), canonical)
    | _ ->
        let expr, canonical =
          read_module_type_maybe_canonical env
            (id :> Identifier.Signature.t)
            container ~canonical md.md_type
        in
        (ModuleType expr, canonical)
  in
  let canonical = (canonical :> Path.Module.t option) in
  let hidden =
#if OCAML_VERSION >= (4,10,0)
    match canonical, md.md_id with
    | None, Some id -> Odoc_model.Root.contains_double_underscore (Ident.name id)
    | _,_ -> false
#else
    match canonical with
    | None -> Odoc_model.Root.contains_double_underscore (Ident.name md.md_id)
    | _ -> false
#endif
  in
  Some {id; locs; doc; type_; canonical; hidden}

and read_module_declarations env parent mds =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let open Signature in
  List.fold_left
    (fun (acc, recursive) md ->
      let comments = Doc_attr.standalone_multiple container md.md_attributes in
      let comments = List.map (fun com -> Comment com) comments in
      match read_module_declaration env parent md with
      | Some md -> ((Module (recursive, md))::(List.rev_append comments acc), And)
      | None -> acc, recursive)
    ([], Rec) mds
  |> fst
  |> List.rev

and read_module_equation env p =
  let open Module in
    Alias (Env.Path.read_module env p, None)

and read_signature_item env parent item =
  let open Signature in
    match item.sig_desc with
    | Tsig_value vd ->
        [read_value_description env parent vd]
#if OCAML_VERSION < (4,3,0)
    | Tsig_type decls ->
      let rec_flag = Ordinary in
#else
    | Tsig_type (rec_flag, decls) ->
      let rec_flag =
        match rec_flag with
        | Recursive -> Ordinary
        | Nonrecursive -> Nonrec
      in
#endif
      read_type_declarations env parent rec_flag decls
    | Tsig_typext tyext ->
        [TypExt (read_type_extension env parent tyext)]
    | Tsig_exception ext ->
#if OCAML_VERSION >= (4,8,0)
        [Exception (read_exception env parent ext.tyexn_constructor)]
#else
        [Exception (read_exception env parent ext)]
#endif
    | Tsig_module md -> begin
        match read_module_declaration env parent md with
        | Some m -> [Module (Ordinary, m)]
        | None -> []
        end
    | Tsig_recmodule mds ->
        read_module_declarations env parent mds
    | Tsig_modtype mtd ->
        [ModuleType (read_module_type_declaration env parent mtd)]
    | Tsig_open o ->
        [
          Open (read_open env parent o)
        ]
    | Tsig_include incl ->
        read_include env parent incl
    | Tsig_class cls ->
        read_class_descriptions env parent cls
    | Tsig_class_type cltyps ->
        read_class_type_declarations env parent cltyps
    | Tsig_attribute attr -> begin
        let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
          match Doc_attr.standalone container attr with
          | None -> []
          | Some doc -> [Comment doc]
      end
#if OCAML_VERSION >= (4,8,0)
    | Tsig_typesubst tst ->
        read_type_substitutions env parent tst
    | Tsig_modsubst mst ->
        [ModuleSubstitution (read_module_substitution env parent mst)]
#if OCAML_VERSION >= (4,13,0)
    | Tsig_modtypesubst mtst ->
        [ModuleTypeSubstitution (read_module_type_substitution env parent mtst)]
#endif


and read_module_substitution env parent ms =
  let open ModuleSubstitution in
  let id = Env.find_module_identifier env ms.ms_id in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, () = Doc_attr.attached Odoc_model.Semantics.Expect_none container ms.ms_attributes in
  let manifest = Env.Path.read_module env ms.ms_manifest in
  { id; doc; manifest }

#if OCAML_VERSION >= (4,13,0)
and read_module_type_substitution env parent mtd =
  let open ModuleTypeSubstitution in
  let id = Env.find_module_type env mtd.mtd_id in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, () = Doc_attr.attached Odoc_model.Semantics.Expect_none container mtd.mtd_attributes in
  let expr = match opt_map (read_module_type env (id :> Identifier.Signature.t) container) mtd.mtd_type with
    | None -> assert false
    | Some x -> x
  in
  {id; doc; manifest=expr;}
#endif


#endif

and read_include env parent incl =
  let open Include in
  let loc = Doc_attr.read_location incl.incl_loc in
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc, status = Doc_attr.attached Odoc_model.Semantics.Expect_status container incl.incl_attributes in
  let content, shadowed = Cmi.read_signature_noenv env parent (Odoc_model.Compat.signature incl.incl_type) in
  let expr = read_module_type env parent container incl.incl_mod in
  let umty = Odoc_model.Lang.umty_of_mty expr in 
  let expansion = { content; shadowed; } in
  match umty with
  | Some uexpr ->
    let decl = Include.ModuleType uexpr in
    [Include {parent; doc; decl; expansion; status; strengthened=None; loc }]
  | _ ->
    content.items

and read_open env parent o =
  let container = (parent : Identifier.Signature.t :> Identifier.LabelParent.t) in
  let doc = Doc_attr.attached_no_tag container o.open_attributes in
  #if OCAML_VERSION >= (4,8,0)
  let signature = o.open_bound_items in
  #else
  let signature = [] in
  #endif
  let expansion, _ = Cmi.read_signature_noenv env parent (Odoc_model.Compat.signature signature) in
  { expansion; doc }

and read_signature :
      'tags. 'tags Odoc_model.Semantics.handle_internal_tags -> _ -> _ -> _ ->
      _ * 'tags =
 fun internal_tags env parent sg ->
  let env = Env.add_signature_tree_items parent sg env in
  let items, (doc, doc_post), tags =
    let classify item =
      match item.sig_desc with
      | Tsig_attribute attr -> Some (`Attribute attr)
      | Tsig_open _ -> Some `Open
      | _ -> None
    in
    Doc_attr.extract_top_comment internal_tags ~classify parent sg.sig_items
  in
  let items =
    List.fold_left
      (fun items item ->
        List.rev_append (read_signature_item env parent item) items)
      [] items
    |> List.rev
  in
  match doc_post with
  | [] ->
    ({ Signature.items; compiled = false; doc }, tags)
  | _ ->
    ({ Signature.items = Comment (`Docs doc_post) :: items; compiled=false; doc }, tags)

let read_interface root name intf =
  let id = Identifier.Mk.root (root, Odoc_model.Names.ModuleName.make_std name) in
  let sg, canonical =
    read_signature Odoc_model.Semantics.Expect_canonical (Env.empty ()) id intf
  in
  (id, sg, (canonical :> Odoc_model.Paths.Path.Module.t option))
