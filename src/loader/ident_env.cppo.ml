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

open Odoc_model
open Names

module Id = Paths.Identifier
module P = Paths.Path

type type_ident = Paths.Identifier.Path.Type.t

module LocHashtbl = Hashtbl.Make(struct
    type t = Location.t
    let equal l1 l2 = l1 = l2
    let hash = Hashtbl.hash
  end)

type t =
  { modules : Id.Module.t Ident.tbl;
    parameters : Id.FunctorParameter.t Ident.tbl;
    module_paths : P.Module.t Ident.tbl;
    module_types : Id.ModuleType.t Ident.tbl;
    types : Id.DataType.t Ident.tbl;
    exceptions: Id.Exception.t Ident.tbl;
    extensions: Id.Extension.t Ident.tbl;
    constructors: Id.Constructor.t Ident.tbl;
    values: Id.Value.t Ident.tbl;
    classes : Id.Class.t Ident.tbl;
    class_types : Id.ClassType.t Ident.tbl;
    loc_to_ident : Id.t LocHashtbl.t;
    hidden : Ident.t list; (* we use term hidden to mean shadowed and idents_in_doc_off_mode items*)
  }

let empty () =
  { modules = Ident.empty;
    parameters = Ident.empty;
    module_paths = Ident.empty;
    module_types = Ident.empty;
    types = Ident.empty;
    exceptions = Ident.empty;
    constructors = Ident.empty;
    extensions = Ident.empty;
    values = Ident.empty;
    classes = Ident.empty;
    class_types = Ident.empty;
    loc_to_ident = LocHashtbl.create 100;
    hidden = [];
  }

(* The boolean is an override for whether it should be hidden - true only for
   items introduced by extended open *)
type item = [
    `Module of Ident.t * bool * Location.t option
  | `ModuleType of Ident.t * bool * Location.t option
  | `Type of Ident.t * bool * Location.t option
  | `Constructor of Ident.t * Ident.t * Location.t option
  (* Second ident.t is for the type parent *)
  | `Value of Ident.t * bool * Location.t option
  | `Class of Ident.t * Ident.t * Ident.t * Ident.t option * bool * Location.t option
  | `ClassType of Ident.t * Ident.t * Ident.t option * bool * Location.t option
  | `Exception of Ident.t * Location.t option
  (* Exceptions needs to be added to the [loc_to_ident] table. *)
  | `Extension of Ident.t * Location.t option
  (* Extension constructor also need to be added to the [loc_to_ident] table,
     since they get an entry in the [uid_to_loc] table. *)
]

type items =
  [ item
  | `Include of item list
]

let rec extract_signature_type_items vis items =
  let open Compat in
    match items with
    | Sig_type(id, td, _, vis') :: rest when vis=vis' ->
      if Btype.is_row_name (Ident.name id)
      then extract_signature_type_items vis rest
      else
        let constrs = match td.type_kind with
#if OCAML_VERSION < (5,2,0)
          | Types.Type_abstract -> []
#else
          | Types.Type_abstract _ -> []
#endif
          | Type_record (_, _) -> []
#if OCAML_VERSION < (4,13,0)
          | Type_variant cstrs ->
#else
          | Type_variant (cstrs, _) ->
#endif
            List.map (fun c -> `Constructor (c.Types.cd_id, id, Some c.cd_loc)) cstrs
          | Type_open -> [] in
        `Type (id, vis'=Hidden, None) :: constrs @ extract_signature_type_items vis rest 

    | Sig_module(id, _, _, _, vis') :: rest when vis=vis' ->
      `Module (id, vis'=Hidden, None) :: extract_signature_type_items vis rest

    | Sig_modtype(id, _, vis') :: rest when vis=vis' ->
      `ModuleType (id, vis'=Hidden, None) :: extract_signature_type_items vis rest
    
    | Sig_value(id, _, vis') :: rest when vis=vis' ->
      `Value (id, vis'=Hidden, None) :: extract_signature_type_items vis rest
#if OCAML_VERSION < (5,1,0)
    | Sig_class(id, _, _, vis') :: Sig_class_type(ty_id, _, _, _)
        :: Sig_type(obj_id, _, _, _) :: Sig_type(cl_id, _, _, _) :: rest when vis=vis' ->
      `Class (id, ty_id, obj_id, Some cl_id, vis'=Hidden, None) :: extract_signature_type_items vis rest

    | Sig_class_type(id, _, _, vis') :: Sig_type(obj_id, _, _, _)
      :: Sig_type(cl_id, _, _, _) :: rest when vis=vis' ->
      `ClassType (id, obj_id, Some cl_id, vis'=Hidden, None) :: extract_signature_type_items vis rest
#else
    | Sig_class(id, _, _, vis') :: Sig_class_type(ty_id, _, _, _)
        :: Sig_type(obj_id, _, _, _) :: rest when vis=vis' ->
      `Class (id, ty_id, obj_id, None, vis'=Hidden, None) :: extract_signature_type_items vis rest

    | Sig_class_type(id, _, _, vis') :: Sig_type(obj_id, _, _, _)  :: rest when vis=vis' ->
      `ClassType (id, obj_id, None, vis'=Hidden, None) :: extract_signature_type_items vis rest
#endif

    | Sig_typext (id, constr, Text_exception, vis') :: rest when vis=vis' ->
      `Exception (id, Some constr.ext_loc)
      :: extract_signature_type_items vis rest

    | Sig_typext (id, constr, _, vis') :: rest when vis=vis'->
      `Extension (id, Some constr.ext_loc)
      :: extract_signature_type_items vis rest

    | Sig_class_type(_, _, _, _) :: Sig_type(_, _, _, _)
      :: Sig_type(_, _, _, _) :: rest
    | Sig_class(_, _, _, _) :: Sig_class_type(_, _, _, _)
        :: Sig_type(_, _, _, _) :: Sig_type(_, _, _, _) :: rest
    | Sig_typext (_,_,_,_) :: rest
    | Sig_modtype(_, _, _) :: rest
    | Sig_module(_, _, _, _, _) :: rest
    | Sig_type(_, _, _, _) :: rest
    | Sig_value (_, _, _) :: rest ->
        extract_signature_type_items vis rest

    | Sig_class _ :: _
    | Sig_class_type _ :: _ -> assert false

    | [] -> []

#if OCAML_VERSION >= (4,8,0)

let extract_extended_open o =
  let open Typedtree in
  extract_signature_type_items Hidden (Compat.signature o.open_bound_items)
#endif


let concat_map f l =
  let rec aux f acc = function
    | [] -> List.rev acc
    | x :: l ->
       let xs = f x in
       aux f (List.rev_append xs acc) l
  in aux f [] l

let rec extract_signature_tree_items : bool -> Typedtree.signature_item list -> items list = fun hide_item items ->
  let open Typedtree in
  match items with
#if OCAML_VERSION < (4,3,0)
  | { sig_desc = Tsig_type decls; _} :: rest ->
#else
  | { sig_desc = Tsig_type (_, decls); _} :: rest ->
#endif
    concat_map (fun decl ->
      if Btype.is_row_name (Ident.name decl.typ_id)
      then []
      else
        `Type (decl.typ_id, hide_item, Some decl.typ_loc) ::
        match decl.typ_kind with
          Ttype_abstract -> []
        | Ttype_variant constrs -> List.map (fun c -> `Constructor (c.cd_id, decl.typ_id, Some c.cd_loc)) constrs
        | Ttype_record _ -> []
        | Ttype_open -> []
          )
      decls @ extract_signature_tree_items hide_item rest

#if OCAML_VERSION < (4,8,0)
    | { sig_desc = Tsig_exception tyexn_constructor; _ } :: rest ->
#else
    | { sig_desc = Tsig_exception { tyexn_constructor; _ }; _ } :: rest ->
#endif
       `Exception (tyexn_constructor.ext_id, Some tyexn_constructor.ext_loc) :: extract_signature_tree_items hide_item rest

  | { sig_desc = Tsig_typext { tyext_constructors; _ }; _} :: rest ->
    let x = List.map (fun { ext_id; ext_loc; _ } -> `Extension (ext_id, Some ext_loc)) tyext_constructors in
    x @ extract_signature_tree_items hide_item rest


#if OCAML_VERSION >= (4,10,0)
  | { sig_desc = Tsig_module { md_id = Some id; _ }; sig_loc; _} :: rest ->
      [`Module (id, hide_item, Some sig_loc)] @ extract_signature_tree_items hide_item rest
  | { sig_desc = Tsig_module _; _ } :: rest ->
      extract_signature_tree_items hide_item rest
  | { sig_desc = Tsig_recmodule mds; _} :: rest ->
    List.fold_right (
      fun md items ->
        match md.md_id with
        | Some id -> `Module (id, hide_item, Some md.md_loc) :: items
        | None -> items)
      mds [] @ extract_signature_tree_items hide_item rest
#else 
  | { sig_desc = Tsig_module{ md_id; _}; _} :: rest ->
      [`Module (md_id, hide_item, None)] @ extract_signature_tree_items hide_item rest
  | { sig_desc = Tsig_recmodule mds; _ } :: rest ->
    List.map (fun md -> `Module (md.md_id, hide_item, None))
      mds @ extract_signature_tree_items hide_item rest
#endif
  | { sig_desc = Tsig_value {val_id; _}; sig_loc; _ } :: rest->
      [`Value (val_id, hide_item, Some sig_loc)] @ extract_signature_tree_items hide_item rest 
  | { sig_desc = Tsig_modtype mtd; sig_loc; _} :: rest ->
      [`ModuleType (mtd.mtd_id, hide_item, Some sig_loc)] @ extract_signature_tree_items hide_item rest
  | {sig_desc = Tsig_include incl; _ } :: rest ->
      [`Include (extract_signature_type_items Exported (Compat.signature incl.incl_type))] @ extract_signature_tree_items hide_item rest
  | {sig_desc = Tsig_attribute attr; _ } :: rest ->
      let hide_item = if Doc_attr.is_stop_comment attr then not hide_item else hide_item in
      extract_signature_tree_items hide_item rest
  | {sig_desc = Tsig_class cls; _} :: rest ->
      List.map
        (fun cld ->
            let typehash =
#if OCAML_VERSION < (4,4,0)
            Some cld.ci_id_typesharp
#elif OCAML_VERSION < (5,1,0)
            Some cld.ci_id_typehash
#else
            None
#endif
          in
          `Class (cld.ci_id_class, cld.ci_id_class_type, cld.ci_id_object, typehash, hide_item, Some cld.ci_id_name.loc))
            cls @ extract_signature_tree_items hide_item rest
  | { sig_desc = Tsig_class_type cltyps; _ } :: rest ->
    List.map
      (fun clty ->
          let typehash =
#if OCAML_VERSION < (4,4,0)
            Some clty.ci_id_typesharp
#elif OCAML_VERSION < (5,1,0)
            Some clty.ci_id_typehash
#else
            None
#endif
            in
            
            `ClassType (clty.ci_id_class_type, clty.ci_id_object, typehash, hide_item, Some clty.ci_id_name.loc))
              cltyps @ extract_signature_tree_items hide_item rest
#if OCAML_VERSION >= (4,8,0)
    | { sig_desc = Tsig_modsubst ms; sig_loc; _ } :: rest ->
      [`Module (ms.ms_id, hide_item, Some sig_loc )] @ extract_signature_tree_items hide_item rest
    | { sig_desc = Tsig_typesubst ts; sig_loc; _} :: rest ->
      List.map (fun decl -> `Type (decl.typ_id, hide_item, Some sig_loc)) 
        ts @ extract_signature_tree_items hide_item rest
#endif
#if OCAML_VERSION >= (4,13,0)
    | { sig_desc = Tsig_modtypesubst mtd; sig_loc; _ } :: rest ->
      [`ModuleType (mtd.mtd_id, hide_item, Some sig_loc)] @ extract_signature_tree_items hide_item rest
#endif
    | { sig_desc = Tsig_open _;_} :: rest -> extract_signature_tree_items hide_item rest
    | [] -> []

let rec read_pattern hide_item pat =
  let open Typedtree in
  match pat.pat_desc with
#if OCAML_VERSION < (5,2,0)
  | Tpat_var(id, loc) ->
#else
  | Tpat_var(id, loc, _) ->
#endif
    [`Value(id, hide_item, Some loc.loc)]
#if OCAML_VERSION < (5,2,0)
  | Tpat_alias(pat, id, loc) ->
#else
  | Tpat_alias(pat, id, loc, _) ->
#endif
    `Value(id, hide_item, Some loc.loc) :: read_pattern hide_item pat
  | Tpat_record(pats, _) -> 
      List.concat (List.map (fun (_, _, pat) -> read_pattern hide_item pat) pats)
#if OCAML_VERSION < (4,13,0)
  | Tpat_construct(_, _, pats)
#else
  | Tpat_construct(_, _, pats, _)
#endif
  | Tpat_array pats
  | Tpat_tuple pats -> List.concat (List.map (fun pat -> read_pattern hide_item pat) pats)
  | Tpat_or(pat, _, _)
  | Tpat_variant(_, Some pat, _)
  | Tpat_lazy pat -> read_pattern hide_item pat
  | Tpat_any | Tpat_constant _ | Tpat_variant(_, None, _) -> []
#if OCAML_VERSION >= (4,8,0) && OCAML_VERSION < (4,11,0)
  | Tpat_exception pat -> read_pattern hide_item pat
#endif

let rec extract_structure_tree_items : bool -> Typedtree.structure_item list -> items list = fun hide_item items ->
  let open Typedtree in
    match items with
#if OCAML_VERSION < (4,3,0)
    | { str_desc = Tstr_type decls; _ } :: rest ->
#else
    | { str_desc = Tstr_type (_, decls); _ } :: rest -> (* TODO: handle rec_flag *)
#endif
  concat_map (fun decl ->
      `Type (decl.typ_id, hide_item, Some decl.typ_loc) ::
        (match decl.typ_kind with
          Ttype_abstract -> []
        | Ttype_variant constrs -> List.map (fun c -> `Constructor (c.cd_id, decl.typ_id, Some c.cd_loc)) constrs
        | Ttype_record _ -> []
        | Ttype_open -> []
          ))
           decls @ extract_structure_tree_items hide_item rest

#if OCAML_VERSION < (4,8,0)
    | { str_desc = Tstr_exception tyexn_constructor; _ } :: rest ->
#else
    | { str_desc = Tstr_exception { tyexn_constructor; _ }; _ } :: rest ->
#endif
       `Exception (tyexn_constructor.ext_id, Some tyexn_constructor.ext_loc) :: extract_structure_tree_items hide_item rest

  | { str_desc = Tstr_typext { tyext_constructors; _ }; _} :: rest ->
    let x = List.map (fun { ext_id; ext_loc; _ } -> `Extension (ext_id, Some ext_loc)) tyext_constructors in
    x @ extract_structure_tree_items hide_item rest

#if OCAML_VERSION < (4,3,0)
    | { str_desc = Tstr_value (_, vbs ); _} :: rest ->
#else
    | { str_desc = Tstr_value (_, vbs); _ } :: rest -> (*TODO: handle rec_flag *)
#endif
   ( List.map (fun vb -> read_pattern hide_item vb.vb_pat) vbs
      |> List.flatten) @ extract_structure_tree_items hide_item rest

#if OCAML_VERSION >= (4,10,0)
    | { str_desc = Tstr_module { mb_id = Some id; mb_loc; _}; _} :: rest ->
      [`Module (id, hide_item, Some mb_loc)] @ extract_structure_tree_items hide_item rest
    | { str_desc = Tstr_module _; _} :: rest -> extract_structure_tree_items hide_item rest
    | { str_desc = Tstr_recmodule mbs; _ } :: rest ->
        List.fold_right 
          (fun mb items ->
            match mb.mb_id with
            | Some id -> `Module (id, hide_item, Some mb.mb_loc) :: items
            | None -> items) mbs [] @ extract_structure_tree_items hide_item rest
#else
    | { str_desc = Tstr_module { mb_id; mb_loc; _}; _} :: rest ->
        [`Module (mb_id, hide_item, Some mb_loc)] @ extract_structure_tree_items hide_item rest
    | { str_desc = Tstr_recmodule mbs; _} :: rest ->
        List.map (fun mb -> `Module (mb.mb_id, hide_item, None))
          mbs @ extract_structure_tree_items hide_item rest
#endif
    | { str_desc = Tstr_modtype mtd; str_loc; _} :: rest ->
        [`ModuleType (mtd.mtd_id, hide_item, Some str_loc)] @ extract_structure_tree_items hide_item rest
    | { str_desc = Tstr_include incl; _ } :: rest ->
        [`Include (extract_signature_type_items Exported (Compat.signature incl.incl_type))] @ extract_structure_tree_items hide_item rest
    | { str_desc = Tstr_attribute attr; _} :: rest ->
        let hide_item = if Doc_attr.is_stop_comment attr then not hide_item else hide_item in
        extract_structure_tree_items hide_item rest
    | { str_desc = Tstr_class cls; _ } :: rest ->
        List.map
#if OCAML_VERSION < (4,3,0)
          (fun (cld, _, _) ->
#else
          (fun (cld, _) ->
#endif
             `Class (cld.ci_id_class,
               cld.ci_id_class_type, cld.ci_id_object,
#if OCAML_VERSION < (4,4,0)
               Some cld.ci_id_typesharp,
#elif OCAML_VERSION < (5,1,0)
               Some cld.ci_id_typehash,
#else
               None,
#endif
              hide_item, Some cld.ci_id_name.loc
             )) cls @ extract_structure_tree_items hide_item rest
    | {str_desc = Tstr_class_type cltyps; _ } :: rest ->
        List.map
          (fun (_, _, clty) ->
             `ClassType (clty.ci_id_class_type,
               clty.ci_id_object,
#if OCAML_VERSION < (4,4,0)
               Some clty.ci_id_typesharp,
#elif OCAML_VERSION < (5,1,0)
               Some clty.ci_id_typehash,
#else
               None,
#endif
              hide_item, Some clty.ci_id_name.loc
             )) cltyps @ extract_structure_tree_items hide_item rest
#if OCAML_VERSION < (4,8,0)
    | { str_desc = Tstr_open _; _} :: rest -> extract_structure_tree_items hide_item rest
#else
    | { str_desc = Tstr_open o; _ } :: rest ->
        ((extract_extended_open o) :> items list)  @ extract_structure_tree_items hide_item rest
#endif
    | { str_desc = Tstr_primitive {val_id; _}; str_loc; _ } :: rest ->
      [`Value (val_id, false, Some str_loc)] @ extract_structure_tree_items hide_item rest
    | { str_desc = Tstr_eval _; _} :: rest -> extract_structure_tree_items hide_item rest
    | [] -> []


let flatten_includes : items list -> item list = fun items ->
  List.map (function
    | `Type _
    | `Constructor _
    | `Module _
    | `ModuleType _
    | `Value _
    | `Class _
    | `Exception _
    | `Extension _
    | `ClassType _ as x -> [x]
    | `Include xs -> xs) items |> List.flatten

let type_name_exists name items =
  List.exists (function | `Type (id', _, _) when Ident.name id' = name -> true | _ -> false) items

let value_name_exists name items =
    List.exists (function | `Value (id', _, _) when Ident.name id' = name -> true | _ -> false) items

let module_name_exists name items =
  List.exists (function | `Module (id', _, _) when Ident.name id' = name -> true | _ -> false) items

let module_type_name_exists name items = 
  List.exists (function | `ModuleType (id', _, _) when Ident.name id' = name -> true | _ -> false) items

let class_name_exists name items =
  List.exists (function | `Class (id',_,_,_,_,_) when Ident.name id' = name -> true | _ -> false) items

let class_type_name_exists name items =
  List.exists (function | `ClassType (id',_,_,_,_) when Ident.name id' = name -> true | _ -> false) items

let add_items : Id.Signature.t -> item list -> t -> t = fun parent items env ->
  let open Odoc_model.Paths.Identifier in
  let rec inner items env =
    match items with
    | `Type (t, is_hidden_item, loc) :: rest ->
      let name = Ident.name t in
      let is_hidden = is_hidden_item || type_name_exists name rest in
        let identifier, hidden =
        if is_hidden
        then Mk.type_(parent, TypeName.internal_of_string name), t :: env.hidden
        else Mk.type_(parent, TypeName.make_std name), env.hidden
      in
      let types = Ident.add t identifier env.types in
      (match loc with | Some l -> LocHashtbl.add env.loc_to_ident l (identifier :> Id.any) | _ -> ());
      inner rest { env with types; hidden }

    | `Constructor (t, t_parent, loc) :: rest ->
      let name = Ident.name t in
      let identifier =
        let parent = Ident.find_same t_parent env.types  in
        Mk.constructor(parent, ConstructorName.make_std name)
      in
      let constructors = Ident.add t identifier env.constructors in
      (match loc with | Some l -> LocHashtbl.add env.loc_to_ident l (identifier :> Id.any) | _ -> ());
      inner rest { env with constructors }

    | `Exception (t, loc) :: rest ->
      let name = Ident.name t in
      let identifier = Mk.exception_(parent, ExceptionName.make_std name) in
      (match loc with | Some l -> LocHashtbl.add env.loc_to_ident l (identifier :> Id.any) | _ -> ());
      let exceptions = Ident.add t identifier env.exceptions in
      inner rest {env with exceptions }

    | `Extension (t, loc) :: rest ->
      let name = Ident.name t in
      let identifier = Mk.extension(parent, ExtensionName.make_std name) in
      (match loc with | Some l -> LocHashtbl.add env.loc_to_ident l (identifier :> Id.any) | _ -> ());
      let extensions = Ident.add t identifier env.extensions in
      inner rest {env with extensions }

    | `Value (t, is_hidden_item, loc) :: rest ->
      let name = Ident.name t in
      let is_hidden = is_hidden_item || value_name_exists name rest in
      let identifier, hidden =
        if is_hidden
        then Mk.value(parent, ValueName.internal_of_string name), t :: env.hidden
        else Mk.value(parent, ValueName.make_std name), env.hidden
      in
      let values = Ident.add t identifier env.values in
      (match loc with | Some l -> LocHashtbl.add env.loc_to_ident l (identifier :> Id.any) | _ -> ());
      inner rest { env with values; hidden }

    | `ModuleType (t, is_hidden_item, loc) :: rest ->
      let name = Ident.name t in
      let is_hidden = is_hidden_item || module_type_name_exists name rest in
      let identifier, hidden =
        if is_hidden
        then Mk.module_type(parent, ModuleTypeName.internal_of_string name), t :: env.hidden
        else Mk.module_type(parent, ModuleTypeName.make_std name), env.hidden
      in
      let module_types = Ident.add t identifier env.module_types in
      (match loc with | Some l -> LocHashtbl.add env.loc_to_ident l (identifier :> Id.any) | _ -> ());
      inner rest { env with module_types; hidden }

    | `Module (t, is_hidden_item, loc) :: rest ->
      let name = Ident.name t in
      let double_underscore = Odoc_model.Root.contains_double_underscore name in
      let is_hidden = is_hidden_item || module_name_exists name rest || double_underscore in
      let identifier, hidden =
        if is_hidden 
        then Mk.module_(parent, ModuleName.internal_of_string name), t :: env.hidden
        else Mk.module_(parent, ModuleName.make_std name), env.hidden
      in
      let path = `Identifier(identifier, is_hidden) in
      let modules = Ident.add t identifier env.modules in
      let module_paths = Ident.add t path env.module_paths in
      (match loc with | Some l -> LocHashtbl.add env.loc_to_ident l (identifier :> Id.any) | _ -> ());
      inner rest { env with modules; module_paths; hidden }

    | `Class (t,t2,t3,t4, is_hidden_item, loc) :: rest ->
      let name = Ident.name t in
      let is_hidden = is_hidden_item || class_name_exists name rest in
      let class_types = match t4 with
        | None -> [t;t2;t3]
        | Some t4 -> [t;t2;t3;t4]
      in
      let identifier, hidden =
        if is_hidden 
        then Mk.class_(parent, ClassName.internal_of_string name), class_types @ env.hidden
        else Mk.class_(parent, ClassName.make_std name), env.hidden
      in

      let classes =
        List.fold_right (fun id classes -> Ident.add id identifier classes)
          class_types env.classes in
      
      (match loc with | Some l -> LocHashtbl.add env.loc_to_ident l (identifier :> Id.any) | _ -> ());

      inner rest { env with classes; hidden }

    | `ClassType (t,t2,t3, is_hidden_item, loc) :: rest ->
      let name = Ident.name t in
      let is_hidden = is_hidden_item || class_type_name_exists name rest in
      let class_types = match t3 with
        | None -> [t;t2]
        | Some t3 -> [t;t2;t3]
      in
      let identifier, hidden =
        if is_hidden 
        then Mk.class_type(parent, ClassTypeName.internal_of_string name), class_types @ env.hidden
        else Mk.class_type(parent, ClassTypeName.make_std name), env.hidden
      in
      let class_types =
        List.fold_right (fun id class_types -> Ident.add id identifier class_types)
          class_types env.class_types in
      (match loc with | Some l -> LocHashtbl.add env.loc_to_ident l (identifier :> Id.any) | _ -> ());
      inner rest { env with class_types; hidden }

    | [] -> env
    in inner items env

let identifier_of_loc : t -> Location.t -> Odoc_model.Paths.Identifier.t option = fun env loc ->
  try Some (LocHashtbl.find env.loc_to_ident loc) with Not_found -> None

let iter_located_identifier : t -> (Location.t -> Odoc_model.Paths.Identifier.t -> unit) -> unit = fun env f ->
  LocHashtbl.iter f env.loc_to_ident

let add_signature_tree_items : Paths.Identifier.Signature.t -> Typedtree.signature -> t -> t = 
  fun parent sg env ->
    let items = extract_signature_tree_items false sg.sig_items |> flatten_includes in
    add_items parent items env

let add_structure_tree_items : Paths.Identifier.Signature.t -> Typedtree.structure -> t -> t =
  fun parent sg env ->
  let items = extract_structure_tree_items false sg.str_items |> flatten_includes in
  add_items parent items env

let handle_signature_type_items : Paths.Identifier.Signature.t -> Compat.signature -> t -> t =
  fun parent sg env ->
    let items = extract_signature_type_items Exported sg in
    add_items parent items env

let add_parameter parent id name env =
  let hidden = ModuleName.is_hidden name in
  let oid = Odoc_model.Paths.Identifier.Mk.parameter(parent, name) in
  let path = `Identifier (oid, hidden) in
  let module_paths = Ident.add id path env.module_paths in
  let modules = Ident.add id oid env.modules in
  let parameters = Ident.add id oid env.parameters in
  { env with module_paths; modules; parameters }

let find_module env id =
  Ident.find_same id env.module_paths

let find_module_identifier env id =
  Ident.find_same id env.modules

let find_parameter_identifier env id =
  Ident.find_same id env.parameters

let find_module_type env id =
  Ident.find_same id env.module_types

let find_type_identifier env id =
  Ident.find_same id env.types

let find_constructor_identifier env id =
  Ident.find_same id env.constructors

let find_exception_identifier env id =
  Ident.find_same id env.exceptions

let find_extension_identifier env id =
  Ident.find_same id env.extensions

let find_value_identifier env id =
  Ident.find_same id env.values

(** Lookup a type in the environment. If it isn't found, it's assumed to be a
    core type. *)
let find_type env id =
  try (Ident.find_same id env.types :> Id.Path.Type.t)
  with Not_found -> (
    try (Ident.find_same id env.classes :> Id.Path.Type.t)
    with Not_found -> (
      try (Ident.find_same id env.class_types :> Id.Path.Type.t)
      with Not_found ->
        (Paths.Identifier.Mk.core_type (Ident.name id) :> type_ident)))

let find_class_type env id =
  try
    (Ident.find_same id env.classes :> Id.Path.ClassType.t)
  with Not_found ->
    (Ident.find_same id env.class_types :> Id.Path.ClassType.t)

let find_class_identifier env id =
  Ident.find_same id env.classes

let find_class_type_identifier env id =
  Ident.find_same id env.class_types

let is_shadowed
 env id =
    List.mem id env.hidden
module Path = struct

  let read_module_ident env id =
    if Ident.persistent id then `Root (Ident.name id)
    else
      try find_module env id
      with Not_found -> assert false

  let read_module_type_ident env id =
    try
      `Identifier (find_module_type env id, false)
    with Not_found -> assert false

  let read_type_ident env id =
    `Identifier (find_type env id, false)

  let read_class_type_ident env id : Paths.Path.ClassType.t =
    try
      `Identifier (find_class_type env id, false)
    with Not_found ->
      `Dot(`Root "*", (Ident.name id))
      (* TODO remove this hack once the fix for PR#6650
         is in the OCaml release *)

  (*  When a type is a classtype path (with a #), the # is stripped off because
      each ident is mapped to the identifier named for the ident without a
      hash. e.g. in the following, we take the name of the identifier from
      cd_id_class, and therefore even [Pident #u/10] will map to identifier
      [u].
      
      Typedtree.Tsig_class_type
      [{Typedtree.ci_virt = Asttypes.Concrete; ci_params = [];
        ci_id_name = {Asttypes.txt = ...; loc = ...}; ci_id_class = u/13[14];
        ci_id_class_type = u/12[14]; ci_id_object = u/11[14];
        ci_id_typehash = #u/10[14];
      
      For a dotted path though, we have to strip the # off manually here, so
      [read_class_type] and [read_type] both need the following function. 
  *)
  let strip_hash s =
    if s.[0]='#' then String.sub s 1 (String.length s - 1) else s

  let rec read_module : t -> Path.t -> Paths.Path.Module.t = fun env -> function
    | Path.Pident id -> read_module_ident env id
#if OCAML_VERSION >= (4,8,0)
    | Path.Pdot(p, s) -> `Dot(read_module env p, s)
#else
    | Path.Pdot(p, s, _) -> `Dot(read_module env p, s)
#endif
    | Path.Papply(p, arg) -> `Apply(read_module env p, read_module env arg)
#if OCAML_VERSION >= (5,1,0)
    | Path.Pextra_ty _ -> assert false
#endif

  let read_module_type env = function
    | Path.Pident id -> read_module_type_ident env id
#if OCAML_VERSION >= (4,8,0)
    | Path.Pdot(p, s) -> `Dot(read_module env p, s)
#else
    | Path.Pdot(p, s, _) -> `Dot(read_module env p, s)
#endif
    | Path.Papply(_, _)-> assert false
#if OCAML_VERSION >= (5,1,0)
    | Path.Pextra_ty _ -> assert false
#endif

  let read_class_type env = function
    | Path.Pident id -> read_class_type_ident env id
#if OCAML_VERSION >= (4,8,0)
    | Path.Pdot(p, s) -> `Dot(read_module env p, strip_hash s)
#else
    | Path.Pdot(p, s, _) -> `Dot(read_module env p, strip_hash s)
#endif
    | Path.Papply(_, _)-> assert false
#if OCAML_VERSION >= (5,1,0)
    | Path.Pextra_ty _ -> assert false
#endif

#if OCAML_VERSION < (5,1,0)
  let read_type env = function
#else
    let rec read_type env = function
#endif
    | Path.Pident id -> read_type_ident env id
#if OCAML_VERSION >= (4,8,0)
    | Path.Pdot(p, s) -> `Dot(read_module env p, strip_hash s)
#else
    | Path.Pdot(p, s, _) -> `Dot(read_module env p, strip_hash s)
#endif
    | Path.Papply(_, _)-> assert false
#if OCAML_VERSION >= (5,1,0)
    | Path.Pextra_ty (p,_) -> read_type env p
#endif

end

module Fragment = struct

  let rec read_module : Longident.t -> Paths.Fragment.Module.t = function
    | Longident.Lident s -> `Dot(`Root, s)
    | Longident.Ldot(p, s) -> `Dot((read_module p :> Paths.Fragment.Signature.t), s)
    | Longident.Lapply _ -> assert false

  let read_module_type : Longident.t -> Paths.Fragment.ModuleType.t = function
    | Longident.Lident s -> `Dot(`Root, s)
    | Longident.Ldot(p, s) -> `Dot((read_module p :> Paths.Fragment.Signature.t), s)
    | Longident.Lapply _ -> assert false

  let read_type = function
    | Longident.Lident s -> `Dot(`Root, s)
    | Longident.Ldot(p, s) -> `Dot((read_module p :> Paths.Fragment.Signature.t), s)
    | Longident.Lapply _ -> assert false

end
