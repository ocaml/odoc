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
module P = Paths.Path

type type_ident = Paths.Identifier.Path.Type.t

type t =
  { modules : Id.Module.t Ident.tbl;
    module_paths : P.Module.t Ident.tbl;
    module_types : Id.ModuleType.t Ident.tbl;
    types : Id.DataType.t Ident.tbl;
    values: Id.Value.t Ident.tbl;
    classes : Id.Class.t Ident.tbl;
    class_types : Id.ClassType.t Ident.tbl;
    shadowed : Ident.t list }

let empty =
  { modules = Ident.empty;
    module_paths = Ident.empty;
    module_types = Ident.empty;
    types = Ident.empty;
    values = Ident.empty;
    classes = Ident.empty;
    class_types = Ident.empty;
    shadowed = [] }

(* The boolean is an override for whether it should be hidden - true only for
   items introduced by extended open *)
type extracted_item = [
    `Module of Ident.t * bool
  | `ModuleType of Ident.t * bool
  | `Type of Ident.t * bool
  | `Value of Ident.t * bool
  | `Class of Ident.t * Ident.t * Ident.t * Ident.t * bool
  | `ClassType of Ident.t * Ident.t * Ident.t * bool
]

type extracted_items =
  [ extracted_item
  | `Include of extracted_item list
]

let builtin_idents = List.map snd Predef.builtin_idents


let rec extract_signature_type_items items =
  let open Compat in
    match items with
    | Sig_type(id, _, _, Exported) :: rest ->
      if Btype.is_row_name (Ident.name id)
      then extract_signature_type_items rest
      else `Type (id, false) :: extract_signature_type_items rest 

    | Sig_module(id, _, _, _, Exported) :: rest ->
      `Module (id, false) :: extract_signature_type_items rest

    | Sig_modtype(id, _, Exported) :: rest ->
      `ModuleType (id, false) :: extract_signature_type_items rest
    
    | Sig_value(id, _, Exported) :: rest ->
      `Value (id, false) :: extract_signature_type_items rest

    | Sig_class(id, _, _, Exported) :: Sig_class_type(ty_id, _, _, _)
        :: Sig_type(obj_id, _, _, _) :: Sig_type(cl_id, _, _, _) :: rest ->
      `Class (id, ty_id, obj_id, cl_id, false) :: extract_signature_type_items rest

    | Sig_class_type(id, _, _, Exported) :: Sig_type(obj_id, _, _, _)
      :: Sig_type(cl_id, _, _, _) :: rest ->
      `ClassType (id, obj_id, cl_id, false) :: extract_signature_type_items rest

    | Sig_typext _ :: rest -> 
        extract_signature_type_items rest

    | Sig_class_type(_, _, _, Hidden) :: Sig_type(_, _, _, _)
      :: Sig_type(_, _, _, _) :: rest
    | Sig_class(_, _, _, Hidden) :: Sig_class_type(_, _, _, _)
        :: Sig_type(_, _, _, _) :: Sig_type(_, _, _, _) :: rest
    | Sig_modtype(_, _, Hidden) :: rest
    | Sig_module(_, _, _, _, Hidden) :: rest
    | Sig_type(_, _, _, Hidden) :: rest
    | Sig_value (_, _, Hidden) :: rest ->
        extract_signature_type_items rest

    | Sig_class _ :: _
    | Sig_class_type _ :: _ -> assert false

    | [] -> []

#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08

let rec unwrap_module_expr_desc = function
  | Typedtree.Tmod_constraint(mexpr, _, Tmodtype_implicit, _) ->
      unwrap_module_expr_desc mexpr.mod_desc
  | desc -> desc

let rec extract_extended_open_items items =
  let open Types in
    match items with
    | Sig_type(id, _, _, _) :: rest ->
      if Btype.is_row_name (Ident.name id)
      then extract_extended_open_items rest
      else `Type (id, true) :: extract_extended_open_items rest 

    | Sig_module(id, _, _, _, _) :: rest ->
      `Module (id, true) :: extract_extended_open_items rest

    | Sig_modtype(id, _, _) :: rest ->
      `ModuleType (id, true) :: extract_extended_open_items rest
    
    | Sig_value(id, _, _) :: rest ->
      `Value (id, true) :: extract_extended_open_items rest
    
    | Sig_class(id, _, _, _) :: Sig_class_type(ty_id, _, _, _)
        :: Sig_type(obj_id, _, _, _) :: Sig_type(cl_id, _, _, _) :: rest ->
      `Class (id, ty_id, obj_id, cl_id, true) :: extract_extended_open_items rest

    | Sig_class_type(id, _, _, _) :: Sig_type(obj_id, _, _, _)
      :: Sig_type(cl_id, _, _, _) :: rest ->
      `ClassType (id, obj_id, cl_id, true) :: extract_extended_open_items rest

    |  Sig_typext _ :: rest ->
        extract_extended_open_items rest

    | Sig_class _ :: _
    | Sig_class_type _ :: _ -> assert false

    | [] -> []

let extract_extended_open o =
  let open Typedtree in
  match unwrap_module_expr_desc o.open_expr.mod_desc with
  | Tmod_ident(_, _) -> []
  | _ ->
      extract_extended_open_items o.open_bound_items
#endif


let extract_signature_tree_item item =
  let open Typedtree in
    match item.sig_desc with
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
    | Tsig_type decls ->
#else
    | Tsig_type (_rec_flag, decls) -> (* TODO: handle rec_flag *)
#endif
        List.map (fun decl -> `Type (decl.typ_id, false)) decls

#if OCAML_MAJOR = 4 && OCAML_MINOR >= 10
    | Tsig_module { md_id = Some id; _ } ->
        [`Module (id, false)]
    | Tsig_module _ ->
        []
    | Tsig_recmodule mds ->
        List.fold_right
          (fun md items ->
            match md.md_id with
            | Some id -> `Module (id, false) :: items
            | None -> items)
          mds []
#else
    | Tsig_module { md_id; _ } ->
      [`Module (md_id, false)]
    | Tsig_recmodule mds ->
      List.map (fun md -> `Module (md.md_id, false)) mds
#endif
    | Tsig_value {val_id; _} ->
      [`Value (val_id, false)]
    | Tsig_modtype mtd ->
      [`ModuleType (mtd.mtd_id, false)]
    | Tsig_include incl ->
      [`Include (extract_signature_type_items (Compat.signature incl.incl_type))]
    | Tsig_class cls ->
        List.map
          (fun cld ->
             let typehash =
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
              cld.ci_id_typesharp
#else
              cld.ci_id_typehash
#endif
            in
            `Class (cld.ci_id_class, cld.ci_id_class_type, cld.ci_id_object, typehash, false)) cls
    | Tsig_class_type cltyps ->
      List.map
        (fun clty ->
            let typehash =
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
              clty.ci_id_typesharp
#else
              clty.ci_id_typehash
#endif
            in
            
            `ClassType (clty.ci_id_class_type, clty.ci_id_object, typehash, false )) cltyps
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | Tsig_modsubst ms ->
      [`Module (ms.ms_id, false)]
    | Tsig_typesubst ts ->
      List.map (fun decl -> `Type (decl.typ_id, false)) ts
#endif
    | Tsig_typext _
    | Tsig_exception _ | Tsig_open _
    | Tsig_attribute _ -> []

let extract_signature_tree_items sg =
  let open Typedtree in
    List.map extract_signature_tree_item sg.sig_items |> List.flatten

let rec read_pattern pat =
  let open Typedtree in
  match pat.pat_desc with
  | Tpat_var(id, _) -> [`Value(id, false)]
  | Tpat_alias(pat, id, _) -> `Value(id, false) :: read_pattern pat
  | Tpat_record(pats, _) -> 
    List.concat (List.map (fun (_, _, pat) -> read_pattern pat) pats)
  | Tpat_construct(_, _, pats) 
  | Tpat_array pats
  | Tpat_tuple pats -> List.concat (List.map read_pattern pats)
  | Tpat_or(pat, _, _)
  | Tpat_variant(_, Some pat, _)
  | Tpat_lazy pat -> read_pattern pat
  | Tpat_any | Tpat_constant _ | Tpat_variant(_, None, _) -> []
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08 && OCAML_MINOR < 11
  | Tpat_exception pat -> read_pattern pat
#endif

let extract_structure_tree_item item =
  let open Typedtree in
    match item.str_desc with
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
    | Tstr_type decls ->
#else
    | Tstr_type (_rec_flag, decls) -> (* TODO: handle rec_flag *)
#endif
        List.map (fun decl -> `Type (decl.typ_id, false)) decls


#if OCAML_MAJOR = 4 && OCAML_MINOR < 03
    | Tstr_value (_, vbs )->
#else
    | Tstr_value (_rec_flag, vbs) -> (*TODO: handle rec_flag *)
#endif
    List.map (fun vb -> read_pattern vb.vb_pat) vbs |> List.flatten

#if OCAML_MAJOR = 4 && OCAML_MINOR >= 10
    | Tstr_module { mb_id = Some id; _} ->
      [`Module (id, false)]
    | Tstr_module _ -> []
    | Tstr_recmodule mbs ->
        List.fold_right 
          (fun mb items ->
            match mb.mb_id with
            | Some id -> `Module (id, false) :: items
            | None -> items) mbs []
#else
    | Tstr_module { mb_id; _} -> [`Module (mb_id, false)]
    | Tstr_recmodule mbs ->
        List.map (fun mb -> `Module (mb.mb_id, false)) mbs
#endif
    | Tstr_modtype mtd ->
        [`ModuleType (mtd.mtd_id, false)]
    | Tstr_include incl ->
        [`Include (extract_signature_type_items (Compat.signature incl.incl_type))]
    | Tstr_class cls ->
        List.map
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
          (fun (cld, _, _) ->
#else
          (fun (cld, _) ->
#endif
             `Class (cld.ci_id_class,
               cld.ci_id_class_type, cld.ci_id_object,
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               cld.ci_id_typesharp,
#else
               cld.ci_id_typehash,
#endif
               false
             )) cls
    | Tstr_class_type cltyps ->
        List.map
          (fun (_, _, clty) ->
             `ClassType (clty.ci_id_class_type,
               clty.ci_id_object,
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               clty.ci_id_typesharp,
#else
               clty.ci_id_typehash,
#endif
              false
             )) cltyps
#if OCAML_MAJOR = 4 && OCAML_MINOR < 08
    | Tstr_open _ -> []
#else
    | Tstr_open o ->
      ((extract_extended_open o) :> extracted_items list)
#endif
    | Tstr_eval _
    | Tstr_primitive _ | Tstr_typext _
    | Tstr_exception _
    | Tstr_attribute _ -> []

let extract_structure_tree_items str =
  let open Typedtree in
    List.map extract_structure_tree_item str.str_items |> List.flatten

let flatten_extracted : extracted_items list -> extracted_item list = fun items ->
  List.map (function
    | `Type _ 
    | `Module _
    | `ModuleType _
    | `Value _
    | `Class _
    | `ClassType _ as x -> [x]
    | `Include xs -> xs) items |> List.flatten

let type_name_exists name items =
  List.exists (function | `Type (id', _) when Ident.name id' = name -> true | _ -> false) items

let value_name_exists name items =
    List.exists (function | `Value (id', _) when Ident.name id' = name -> true | _ -> false) items

let module_name_exists name items =
  List.exists (function | `Module (id', _) when Ident.name id' = name -> true | _ -> false) items

let module_type_name_exists name items = 
  List.exists (function | `ModuleType (id', _) when Ident.name id' = name -> true | _ -> false) items

let class_name_exists name items =
  List.exists (function | `Class (id',_,_,_,_) when Ident.name id' = name -> true | _ -> false) items

let class_type_name_exists name items =
  List.exists (function | `ClassType (id',_,_,_) when Ident.name id' = name -> true | _ -> false) items

let env_of_items parent items env =
  let rec inner items env =
    match items with
    | `Type (t,force_shadowed) :: rest ->
      let name = Ident.name t in
      let is_shadowed = force_shadowed || type_name_exists name rest in
        let identifier, shadowed =
        if is_shadowed
        then `Type(parent, TypeName.internal_of_string name), t :: env.shadowed
        else `Type(parent, TypeName.make_std name), env.shadowed
      in
      let types = Ident.add t identifier env.types in      
      inner rest { env with types; shadowed }

    | `Value (t,force_shadowed) :: rest ->
      let name = Ident.name t in
      let is_shadowed = force_shadowed || value_name_exists name rest in
        let identifier, shadowed =
        if is_shadowed
        then `Value(parent, ValueName.internal_of_string name), t :: env.shadowed
        else `Value(parent, ValueName.make_std name), env.shadowed
      in
      let values = Ident.add t identifier env.values in      
      inner rest { env with values; shadowed }

    | `ModuleType (t, force_shadowed) :: rest ->
      let name = Ident.name t in
      let is_shadowed = force_shadowed || module_type_name_exists name rest in
      let identifier, shadowed =
        if is_shadowed 
        then `ModuleType(parent, ModuleTypeName.internal_of_string name), t :: env.shadowed
        else `ModuleType(parent, ModuleTypeName.make_std name), env.shadowed
      in
      let module_types = Ident.add t identifier env.module_types in
      inner rest { env with module_types; shadowed }
    | `Module (t, force_shadowed) :: rest ->
      let name = Ident.name t in
      let is_shadowed = force_shadowed || module_name_exists name rest in
      let identifier, shadowed =
        if is_shadowed 
        then `Module(parent, ModuleName.internal_of_string name), t :: env.shadowed
        else `Module(parent, ModuleName.make_std name), env.shadowed
      in
      let path = `Identifier(identifier, is_shadowed) in
      let modules = Ident.add t identifier env.modules in
      let module_paths = Ident.add t path env.module_paths in
      inner rest { env with modules; module_paths; shadowed }
    | `Class (t,t2,t3,t4,force_shadowed) :: rest ->
      let name = Ident.name t in
      let is_shadowed = force_shadowed || class_name_exists name rest in
      let identifier, shadowed =
        if is_shadowed 
        then `Class(parent, ClassName.internal_of_string name), t :: t2 :: t3 :: t4 :: env.shadowed
        else `Class(parent, ClassName.make_std name), env.shadowed
      in
      let classes =
        List.fold_right (fun id classes -> Ident.add id identifier classes)
          [t; t2; t3; t4] env.classes in
      inner rest { env with classes; shadowed }
    | `ClassType (t,t2,t3,force_shadowed) :: rest ->
      let name = Ident.name t in
      let is_shadowed = force_shadowed || class_type_name_exists name rest in
      let identifier, shadowed =
        if is_shadowed 
        then `ClassType(parent, ClassTypeName.internal_of_string name), t :: t2 :: t3 :: env.shadowed
        else `ClassType(parent, ClassTypeName.make_std name), env.shadowed
      in
      let class_types =
        List.fold_right (fun id class_types -> Ident.add id identifier class_types)
          [t; t2; t3] env.class_types in
      inner rest { env with class_types; shadowed }
    | [] -> env
    in inner items env
  



let add_signature_tree_items : Paths.Identifier.Signature.t -> Typedtree.signature -> t -> t = 
  fun parent sg env ->
    let items = extract_signature_tree_items sg |> flatten_extracted in
    env_of_items parent items env

let add_structure_tree_items : Paths.Identifier.Signature.t -> Typedtree.structure -> t -> t =
  fun parent sg env ->
  let items = extract_structure_tree_items sg |> flatten_extracted in
  env_of_items parent items env

let handle_signature_type_items : Paths.Identifier.Signature.t -> Compat.signature -> t -> t =
  fun parent sg env ->
    let items = extract_signature_type_items sg in
    env_of_items parent items env

let add_parameter parent id name env =
  let hidden = ParameterName.is_hidden name in
  let path = `Identifier (`Parameter(parent, name), hidden) in
  let module_paths = Ident.add id path env.module_paths in
  { env with module_paths }

let find_module env id =
  (* Format.fprintf Format.err_formatter "Finding module path: %a\n%!" (Ident.print_with_scope) id; *)
  Ident.find_same id env.module_paths

let find_module_identifier env id =
  (* Format.fprintf Format.err_formatter "Finding module identifier: %a\n%!" (Ident.print_with_scope) id; *)
  Ident.find_same id env.modules

let find_module_type env id =
  Ident.find_same id env.module_types

let find_type_identifier env id =
  Ident.find_same id env.types

let find_value_identifier env id =
  Ident.find_same id env.values

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

let is_shadowed env id =
  List.mem id env.shadowed

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
    try
      `Identifier (find_type env id, false)
    with Not_found -> assert false

  let read_class_type_ident env id : Paths.Path.ClassType.t =
    try
      `Identifier (find_class_type env id, false)
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
