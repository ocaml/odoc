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

open Predefined
open Names

module Id = Paths.Identifier
module Rp = Paths.Path.Resolved

type type_ident = Paths.Identifier.Path.Type.t

type t =
  { modules : Rp.Module.t Ident.tbl;
    module_ids : Id.Module.t Ident.tbl;
    module_types : Id.ModuleType.t Ident.tbl;
    types : Id.DataType.t Ident.tbl;
    classes : Id.Class.t Ident.tbl;
    class_types : Id.ClassType.t Ident.tbl; }

let empty =
  { modules = Ident.empty;
    module_ids = Ident.empty;
    module_types = Ident.empty;
    types = Ident.empty;
    classes = Ident.empty;
    class_types = Ident.empty; }

let builtin_idents = List.map snd Predef.builtin_idents

let is_shadowing map name idents identifier =
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
  let all = Ident.find_all name map in
  List.filter (fun (ident', identifier') -> not (List.mem ident' idents) && identifier' = identifier) all |> List.map fst
#else
  ignore(map, name, idents, identifier);
  []
#endif

let ident_remove id map =
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
  Ident.remove id map
#else
  let bindings = Ident.fold_all (fun k v acc -> (k,v)::acc) map [] in
  let bindings' = List.remove_assoc id bindings in
  List.fold_left (fun new_map (k, v) -> Ident.add k v new_map) Ident.empty bindings'
#endif

let add_module parent id name env =
  let identifier = `Module(parent, name) in
  let path = if ModuleName.is_hidden name then `Hidden (`Identifier identifier) else `Identifier identifier in
  match is_shadowing env.module_ids (ModuleName.to_string name) [id] identifier with
  | [] ->
    let modules = Ident.add id path env.modules in
    let module_ids = Ident.add id identifier env.module_ids in
    { env with modules; module_ids }
  | _ ->
    let new_ident = `Module (parent, ModuleName.internal_of_string (ModuleName.to_string name)) in
    let new_path = `Hidden (`Identifier new_ident) in
    let module_ids = Ident.add id new_ident env.module_ids in
    let modules = Ident.add id new_path env.modules in
    { env with modules; module_ids }

let remove_module _parent id _name env =
    {env with modules = ident_remove id env.modules; module_ids = ident_remove id env.module_ids }

let add_parameter parent id name env =
  let ident = `Identifier (`Parameter(parent, name)) in
  let module_ = if ParameterName.is_hidden name then `Hidden ident else ident in
  let modules = Ident.add id module_ env.modules in
    { env with modules }

let add_module_type parent id name env =
  let identifier = `ModuleType(parent, name) in
  match is_shadowing env.module_types (ModuleTypeName.to_string name) [id] identifier with
  | [] ->
    let module_types = Ident.add id identifier env.module_types in
    { env with module_types }
  | _ ->
    let new_ident = `ModuleType (parent, ModuleTypeName.internal_of_string (ModuleTypeName.to_string name)) in
    let module_types = Ident.add id new_ident env.module_types in
    { env with module_types }

let remove_module_type _parent id _name env =
  {env with module_types = ident_remove id env.module_types }

let add_type parent id name env =
  let identifier = `Type(parent, name) in
  match is_shadowing env.types (TypeName.to_string name) [id] identifier with
  | [] ->
    let types = Ident.add id identifier env.types in
    { env with types }
  | _ ->
    let new_ident = `Type (parent, TypeName.internal_of_string (TypeName.to_string name)) in
    let types = Ident.add id new_ident env.types in
    { env with types }

let remove_type _parent id _name env =
  { env with types = ident_remove id env.types }

let add_class parent ids name env =
  let identifier = `Class(parent, name) in
  match is_shadowing env.classes (ClassName.to_string name) ids identifier with
  | [] ->
    let classes =
      List.fold_right (fun id -> Ident.add id identifier) ids env.classes in
    { env with classes }
  | _ ->
    let new_ident = `Class (parent, ClassName.internal_of_string (ClassName.to_string name)) in
    let classes = List.fold_right (fun id classes -> Ident.add id new_ident classes) ids env.classes in
    { env with classes }

let remove_class _parent ids _name env =
  { env with classes = List.fold_right ident_remove ids env.classes }

let add_class_type parent ids name env =
  let identifier = `ClassType(parent, name) in
  match is_shadowing env.class_types (ClassTypeName.to_string name) ids identifier with
  | [] ->
    let class_types =
        List.fold_right (fun id -> Ident.add id identifier) ids env.class_types in
    { env with class_types }
  | _ ->
    let new_ident = `ClassType (parent, ClassTypeName.internal_of_string (ClassTypeName.to_string name)) in
    let class_types =
          List.fold_right (fun id class_types -> Ident.add id new_ident class_types) ids env.class_types in
    { env with class_types }

let remove_class_type _parent ids _name env =
  { env with class_types = List.fold_right ident_remove ids env.class_types }

let rec handle_signature_type_items ty parent items env =
  let (handle_type, handle_module, handle_module_type, handle_class, handle_class_type) =
    match ty with
    | `Add -> (add_type, add_module, add_module_type, add_class, add_class_type)
    | `Remove -> (remove_type, remove_module, remove_module_type, remove_class, remove_class_type)
  in
  let open Compat in
    match items with
    | Sig_type(id, _, _, Exported) :: rest ->
        let env = handle_signature_type_items ty parent rest env in
          if Btype.is_row_name (Ident.name id) then env
          else handle_type parent id (TypeName.of_ident id) env
    | Sig_module(id, _, _, _, Exported) :: rest ->
        let env = handle_signature_type_items ty parent rest env in
          handle_module parent id (ModuleName.of_ident id) env
    | Sig_modtype(id, _, Exported) :: rest ->
        let env = handle_signature_type_items ty parent rest env in
          handle_module_type parent id (ModuleTypeName.of_ident id) env
    | Sig_class(id, _, _, Exported) :: Sig_class_type(ty_id, _, _, _)
        :: Sig_type(obj_id, _, _, _) :: Sig_type(cl_id, _, _, _) :: rest ->
        let env = handle_signature_type_items ty parent rest env in
        let name = ClassName.of_ident id in
        handle_class parent [id; ty_id; obj_id; cl_id] name env
    | Sig_class_type(id, _, _, Exported) :: Sig_type(obj_id, _, _, _)
      :: Sig_type(cl_id, _, _, _) :: rest ->
        let env = handle_signature_type_items ty parent rest env in
        let name = ClassTypeName.of_ident id in
          handle_class_type parent [id; obj_id; cl_id] name env
    | (Sig_value _ | Sig_typext _) :: rest -> 
        handle_signature_type_items ty parent rest env

    | Sig_class_type(_, _, _, Hidden) :: Sig_type(_, _, _, _)
      :: Sig_type(_, _, _, _) :: rest
    | Sig_class(_, _, _, Hidden) :: Sig_class_type(_, _, _, _)
        :: Sig_type(_, _, _, _) :: Sig_type(_, _, _, _) :: rest
    | Sig_modtype(_, _, Hidden) :: rest
    | Sig_module(_, _, _, _, Hidden) :: rest
    | Sig_type(_, _, _, Hidden) :: rest ->
        handle_signature_type_items ty parent rest env

    | Sig_class _ :: _
    | Sig_class_type _ :: _ -> assert false

    | [] -> env

#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08

let rec unwrap_module_expr_desc = function
  | Typedtree.Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
      unwrap_module_expr_desc mexpr.mod_desc
  | desc -> desc

let rec add_extended_open_items parent items env =
  let open Types in
    match items with
    | Sig_type(id, _, _, _) :: rest ->
        let env = add_extended_open_items parent rest env in
          if Btype.is_row_name (Ident.name id) then env
          else add_type parent id (TypeName.internal_of_ident id) env
    | Sig_module(id, _, _, _, _) :: rest ->
        let env = add_extended_open_items parent rest env in
          add_module parent id (ModuleName.internal_of_ident id) env
    | Sig_modtype(id, _, _) :: rest ->
        let env = add_extended_open_items parent rest env in
          add_module_type parent id (ModuleTypeName.internal_of_ident id) env
    | Sig_class(id, _, _, _) :: Sig_class_type(ty_id, _, _, _)
        :: Sig_type(obj_id, _, _, _) :: Sig_type(cl_id, _, _, _) :: rest ->
        let env = add_extended_open_items parent rest env in
        let name = ClassName.internal_of_ident id in
        add_class parent [id; ty_id; obj_id; cl_id] name env

    | Sig_class_type(id, _, _, _) :: Sig_type(obj_id, _, _, _)
      :: Sig_type(cl_id, _, _, _) :: rest ->
        let env = add_extended_open_items parent rest env in
      let name = ClassTypeName.internal_of_ident id in
      add_class_type parent [id; obj_id; cl_id] name env

    | (Sig_value _ | Sig_typext _) :: rest ->
        add_extended_open_items parent rest env

    | Sig_class _ :: _
    | Sig_class_type _ :: _ -> assert false

    | [] -> env

let add_extended_open parent o env =
  let open Typedtree in
  match unwrap_module_expr_desc o.open_expr.mod_desc with
  | Tmod_ident(_, _) -> env
  | _ ->
      add_extended_open_items parent o.open_bound_items env
#endif


let add_signature_tree_item parent item env =
  let open Typedtree in
    match item.sig_desc with
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
    | Tsig_type decls ->
#else
    | Tsig_type (_rec_flag, decls) -> (* TODO: handle rec_flag *)
#endif
        List.fold_right
          (fun decl env -> add_type parent decl.typ_id (TypeName.of_ident decl.typ_id) env)
          decls env
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 10
    | Tsig_module { md_id = Some id; _ } ->
        add_module parent id (ModuleName.of_ident id) env
    | Tsig_module _ ->
        env
    | Tsig_recmodule mds ->
        List.fold_right
          (fun md env ->
            match md.md_id with
            | Some id -> add_module parent id (ModuleName.of_ident id) env
            | None -> env)
          mds env
#else
    | Tsig_module { md_id; _ } ->
        add_module parent md_id (ModuleName.of_ident md_id) env
    | Tsig_recmodule mds ->
        List.fold_right
          (fun md env ->
            add_module parent md.md_id (ModuleName.of_ident md.md_id) env)
          mds env
#endif
    | Tsig_modtype mtd ->
        add_module_type parent mtd.mtd_id (ModuleTypeName.of_ident mtd.mtd_id) env
    | Tsig_include incl ->
        handle_signature_type_items `Add parent (Compat.signature incl.incl_type) env
    | Tsig_class cls ->
        List.fold_right
          (fun cld env ->
             let typehash =
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
              cld.ci_id_typesharp
#else
              cld.ci_id_typehash
#endif
            in
            let name = ClassName.of_ident cld.ci_id_class in
            add_class parent [cld.ci_id_class; cld.ci_id_class_type; cld.ci_id_object; typehash] name env)
            cls env
    | Tsig_class_type cltyps ->
        List.fold_right
          (fun clty env ->
            let typehash =
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
              clty.ci_id_typesharp
#else
              clty.ci_id_typehash
#endif
            in
            
            add_class_type parent [clty.ci_id_class_type;
               clty.ci_id_object;
               typehash ]
               (ClassTypeName.of_ident clty.ci_id_class_type)
               env)
          cltyps env
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | Tsig_modsubst ms ->
      add_module parent ms.ms_id (ModuleName.of_ident ms.ms_id) env
    | Tsig_typesubst ts ->
      List.fold_right
        (fun decl env -> add_type parent decl.typ_id (TypeName.of_ident decl.typ_id) env)
        ts env
#endif
    | Tsig_value _ | Tsig_typext _
    | Tsig_exception _ | Tsig_open _
    | Tsig_attribute _ -> env

let add_signature_tree_items parent sg env =
  let open Typedtree in
    List.fold_right (add_signature_tree_item parent) sg.sig_items env

let add_structure_tree_item parent item env =
  let open Typedtree in
    match item.str_desc with
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
    | Tstr_type decls ->
#else
    | Tstr_type (_rec_flag, decls) -> (* TODO: handle rec_flag *)
#endif
        List.fold_right
          (fun decl env -> add_type parent decl.typ_id (TypeName.of_ident decl.typ_id) env)
          decls env
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 10
    | Tstr_module { mb_id = Some id; _} -> add_module parent id (ModuleName.of_ident id) env
    | Tstr_module _ -> env
    | Tstr_recmodule mbs ->
        List.fold_right
          (fun mb env ->
            match mb.mb_id with
            | Some id -> add_module parent id (ModuleName.of_ident id) env
            | None -> env)
          mbs env
#else
    | Tstr_module { mb_id; _} -> add_module parent mb_id (ModuleName.of_ident mb_id) env
    | Tstr_recmodule mbs ->
        List.fold_right
          (fun mb env -> add_module parent mb.mb_id (ModuleName.of_ident mb.mb_id) env)
          mbs env
#endif
    | Tstr_modtype mtd ->
        add_module_type parent mtd.mtd_id (ModuleTypeName.of_ident mtd.mtd_id) env
    | Tstr_include incl ->
        (* Format.fprintf Format.err_formatter "Adding Tstr_include items\n%!"; *)
        let res = handle_signature_type_items `Add parent (Compat.signature incl.incl_type) env in
        (* Format.fprintf Format.err_formatter "Finished adding Tstr_include items\n%!"; *)
        res
    | Tstr_class cls ->
        List.fold_right
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
          (fun (cld, _, _) env ->
#else
          (fun (cld, _) env ->
#endif
             add_class parent [cld.ci_id_class;
               cld.ci_id_class_type; cld.ci_id_object;
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               cld.ci_id_typesharp
#else
               cld.ci_id_typehash
#endif
             ]
               (ClassName.of_ident cld.ci_id_class)
               env)
          cls env
    | Tstr_class_type cltyps ->
        List.fold_right
          (fun (_, _, clty) env ->
             add_class_type parent [clty.ci_id_class_type;
               clty.ci_id_object;
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               clty.ci_id_typesharp
#else
               clty.ci_id_typehash
#endif
             ]
               (ClassTypeName.of_ident clty.ci_id_class_type)
               env)
          cltyps env
#if OCAML_MAJOR = 4 && OCAML_MINOR < 08
    | Tstr_open _ -> env
#else
    | Tstr_open o ->
      add_extended_open parent o env
#endif
    | Tstr_eval _ | Tstr_value _
    | Tstr_primitive _ | Tstr_typext _
    | Tstr_exception _
    | Tstr_attribute _ -> env

let add_structure_tree_items parent str env =
  let open Typedtree in
    List.fold_right (add_structure_tree_item parent) str.str_items env

let find_module env id =
  (* Format.fprintf Format.err_formatter "Finding module path: %a\n%!" (Ident.print_with_scope) id; *)
  Ident.find_same id env.modules

let find_module_identifier env id =
  (* Format.fprintf Format.err_formatter "Finding module identifier: %a\n%!" (Ident.print_with_scope) id; *)
  Ident.find_same id env.module_ids

let find_module_type env id =
  Ident.find_same id env.module_types

let find_type_identifier env id =
  Ident.find_same id env.types

let find_type env id =
  try
    (Ident.find_same id env.types :> Id.Path.Type.t)
  with Not_found ->
    try
      (Ident.find_same id env.classes :> Id.Path.Type.t)
    with Not_found ->
      try
        (Ident.find_same id env.class_types :> Id.Path.Type.t)
      with Not_found ->
        if List.mem id builtin_idents then
            match core_type_identifier (Ident.name id) with
            | Some id -> (id :> type_ident)
            | None -> raise Not_found
        else raise Not_found
        
let find_class_type env id =
  try
    (Ident.find_same id env.classes :> Id.Path.ClassType.t)
  with Not_found ->
    (Ident.find_same id env.class_types :> Id.Path.ClassType.t)

let find_class_identifier env id =
  Ident.find_same id env.classes

let find_class_type_identifier env id =
  Ident.find_same id env.class_types

module Path = struct

  let read_module_ident env id =
    if Ident.persistent id then `Root (Ident.name id)
    else
      try `Resolved (find_module env id)
      with Not_found -> assert false

  let read_module_type_ident env id =
    try
      `Resolved (`Identifier (find_module_type env id))
    with Not_found -> assert false

  let read_type_ident env id =
    try
      `Resolved (`Identifier (find_type env id))
    with Not_found -> assert false

  let read_class_type_ident env id : Paths.Path.ClassType.t =
    try
      `Resolved (`Identifier (find_class_type env id))
    with Not_found ->
      `Dot(`Root "*", (Ident.name id))
      (* TODO remove this hack once the fix for PR#6650
         is in the OCaml release *)

  let rec read_module : t -> Path.t -> Paths.Path.Module.t = fun env -> function
    | Path.Pident id -> read_module_ident env id
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | Path.Pdot(p, s) -> `Dot(read_module env p, s)
#else
    | Path.Pdot(p, s, _) -> `Dot(read_module env p, s)
#endif
    | Path.Papply(p, arg) -> `Apply(read_module env p, read_module env arg)

  let read_module_type env = function
    | Path.Pident id -> read_module_type_ident env id
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | Path.Pdot(p, s) -> `Dot(read_module env p, s)
#else
    | Path.Pdot(p, s, _) -> `Dot(read_module env p, s)
#endif
    | Path.Papply(_, _)-> assert false

  let read_class_type env = function
    | Path.Pident id -> read_class_type_ident env id
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | Path.Pdot(p, s) -> `Dot(read_module env p, s)
#else
    | Path.Pdot(p, s, _) -> `Dot(read_module env p, s)
#endif
    | Path.Papply(_, _)-> assert false

  let read_type env = function
    | Path.Pident id -> read_type_ident env id
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | Path.Pdot(p, s) -> `Dot(read_module env p, s)
#else
    | Path.Pdot(p, s, _) -> `Dot(read_module env p, s)
#endif
    | Path.Papply(_, _)-> assert false

end

module Fragment = struct

  let rec read_module : Longident.t -> Paths.Fragment.Module.t = function
    | Longident.Lident s -> `Dot(`Root, s)
    | Longident.Ldot(p, s) -> `Dot((read_module p :> Paths.Fragment.Signature.t), s)
    | Longident.Lapply _ -> assert false

  let read_type = function
    | Longident.Lident s -> `Dot(`Root, s)
    | Longident.Ldot(p, s) -> `Dot((read_module p :> Paths.Fragment.Signature.t), s)
    | Longident.Lapply _ -> assert false

end
