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

module Id = Paths.Identifier
module Rp = Paths.Path.Resolved

type type_ident = [`Type | `Class | `ClassType] Id.t

type class_type_ident = [`Class | `ClassType] Id.t

type t =
  { modules : Rp.module_ Ident.tbl;
    module_types : Id.module_type Ident.tbl;
    types : type_ident Ident.tbl;
    class_types : class_type_ident Ident.tbl; }

let empty =
  { modules = Ident.empty;
    module_types = Ident.empty;
    types = Ident.empty;
    class_types = Ident.empty; }

let builtin_idents = List.map snd Predef.builtin_idents

let should_be_hidden = Root.contains_double_underscore

let add_module parent id env =
  let name = Ident.name id in
  let ident = Rp.Identifier (Id.Module(parent, name)) in
  let module_ = if should_be_hidden name then Rp.Hidden ident else ident in
  let modules = Ident.add id module_ env.modules in
    { env with modules }

let add_argument parent arg id env =
  let name = Ident.name id in
  let ident = Rp.Identifier (Id.Argument(parent, arg, name)) in
  let module_ = if should_be_hidden name then Rp.Hidden ident else ident in
  let modules = Ident.add id module_ env.modules in
    { env with modules }

let add_module_type parent id env =
  let name = Ident.name id in
  let identifier = Id.ModuleType(parent, name) in
  let module_types = Ident.add id identifier env.module_types in
    { env with module_types }

let add_type parent id env =
  let name = Ident.name id in
  let identifier = Id.Type(parent, name) in
  let types = Ident.add id identifier env.types in
    { env with types }

let add_class parent id ty_id obj_id cl_id env =
  let name = Ident.name id in
  let identifier = Id.Class(parent, name) in
  let add_idents tbl =
    Ident.add id identifier
      (Ident.add ty_id identifier
         (Ident.add obj_id identifier
            (Ident.add cl_id identifier tbl)))
  in
  let types = add_idents env.types in
  let class_types = add_idents env.class_types in
    { env with types; class_types }

let add_class_type parent id obj_id cl_id env =
  let name = Ident.name id in
  let identifier = Id.ClassType(parent, name) in
  let add_idents tbl =
    Ident.add id identifier
         (Ident.add obj_id identifier
            (Ident.add cl_id identifier tbl))
  in
  let types = add_idents env.types in
  let class_types = add_idents env.class_types in
    { env with types; class_types }

let rec add_signature_type_items parent items env =
  let open Types in
    match items with
    | Sig_type(id, _, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          if Btype.is_row_name (Ident.name id) then env
          else add_type parent id env
    | Sig_module(id, _, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          add_module parent id env
    | Sig_modtype(id, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          add_module_type parent id env
    | Sig_class(id, _, _) :: Sig_class_type(ty_id, _, _)
        :: Sig_type(obj_id, _, _) :: Sig_type(cl_id, _, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          add_class parent id ty_id obj_id cl_id env
    | Sig_class _ :: _ -> assert false
    | Sig_class_type(id, _, _) :: Sig_type(obj_id, _, _)
      :: Sig_type(cl_id, _, _) :: rest ->
        let env = add_signature_type_items parent rest env in
          add_class_type parent id obj_id cl_id env
    | Sig_class_type _ :: _ -> assert false
    | (Sig_value _ | Sig_typext _) :: rest ->
        add_signature_type_items parent rest env
    | [] -> env

let add_signature_tree_item parent item env =
  let open Typedtree in
    match item.sig_desc with
    | Tsig_type (_rec_flag, decls) -> (* TODO: remember rec_flag *)
        List.fold_right
          (fun decl env -> add_type parent decl.typ_id env)
          decls env
    | Tsig_module md ->
        add_module parent md.md_id env
    | Tsig_recmodule mds ->
        List.fold_right
          (fun md env -> add_module parent md.md_id env)
          mds env
    | Tsig_modtype mtd ->
        add_module_type parent mtd.mtd_id env
    | Tsig_include incl ->
        add_signature_type_items parent incl.incl_type env
    | Tsig_class cls ->
        List.fold_right
          (fun cld env ->
             add_class parent cld.ci_id_class
               cld.ci_id_class_type cld.ci_id_object
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               cld.ci_id_typesharp
#else
               cld.ci_id_typehash
#endif
               env)
          cls env
    | Tsig_class_type cltyps ->
        List.fold_right
          (fun clty env ->
             add_class_type parent clty.ci_id_class_type
               clty.ci_id_object
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               clty.ci_id_typesharp
#else
               clty.ci_id_typehash
#endif
               env)
          cltyps env
    | Tsig_value _ | Tsig_typext _
    | Tsig_exception _ | Tsig_open _
    | Tsig_attribute _ -> env

let add_signature_tree_items parent sg env =
  let open Typedtree in
    List.fold_right
      (add_signature_tree_item parent)
      sg.sig_items env

let add_structure_tree_item parent item env =
  let open Typedtree in
    match item.str_desc with
    | Tstr_type (_rec_flag, decls) -> (* TODO: remember rec_flag *)
        List.fold_right
          (fun decl env -> add_type parent decl.typ_id env)
          decls env
    | Tstr_module mb -> add_module parent mb.mb_id env
    | Tstr_recmodule mbs ->
        List.fold_right
          (fun mb env -> add_module parent mb.mb_id env)
          mbs env
    | Tstr_modtype mtd ->
        add_module_type parent mtd.mtd_id env
    | Tstr_include incl ->
        add_signature_type_items parent incl.incl_type env
    | Tstr_class cls ->
        List.fold_right
          (fun (cld, _) env ->
             add_class parent cld.ci_id_class
               cld.ci_id_class_type cld.ci_id_object
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               cld.ci_id_typesharp
#else
               cld.ci_id_typehash
#endif
               env)
          cls env
    | Tstr_class_type cltyps ->
        List.fold_right
          (fun (_, _, clty) env ->
             add_class_type parent clty.ci_id_class_type
               clty.ci_id_object
#if OCAML_MAJOR = 4 && OCAML_MINOR < 04
               clty.ci_id_typesharp
#else
               clty.ci_id_typehash
#endif
               env)
          cltyps env
    | Tstr_eval _ | Tstr_value _
    | Tstr_primitive _ | Tstr_typext _
    | Tstr_exception _ | Tstr_open _
    | Tstr_attribute _ -> env

let add_structure_tree_items parent str env =
  let open Typedtree in
    List.fold_right
      (add_structure_tree_item parent)
      str.str_items env

let find_module env id =
  Ident.find_same id env.modules

let find_module_type env id =
  Ident.find_same id env.module_types

let find_type env id =
  try
    Ident.find_same id env.types
  with Not_found ->
    if List.mem id builtin_idents then
        match core_type_identifier (Ident.name id) with
        | Some id -> id
        | None -> raise Not_found
    else raise Not_found

let find_class_type env id =
  Ident.find_same id env.class_types

module Path = struct

  open Paths.Path.Resolved
  open Paths.Path

  let read_module_ident env id =
    if Ident.persistent id then Root (Ident.name id)
    else
      try Resolved (find_module env id)
      with Not_found -> assert false

  let read_module_type_ident env id =
    try
      Resolved (Identifier (find_module_type env id))
    with Not_found -> assert false

  let read_type_ident env id =
    try
      Resolved (Identifier (find_type env id))
    with Not_found -> assert false

  let read_class_type_ident env id : class_type =
    try
      Resolved (Identifier (find_class_type env id))
    with Not_found ->
      Dot(Root "*", (Ident.name id))
      (* TODO remove this hack once the fix for PR#6650
         is in the OCaml release *)

  let rec read_module env = function
    | Path.Pident id -> read_module_ident env id
    | Path.Pdot(p, s, _) -> Dot(read_module env p, s)
    | Path.Papply(p, arg) -> Apply(read_module env p, read_module env arg)

  let read_module_type env = function
    | Path.Pident id -> read_module_type_ident env id
    | Path.Pdot(p, s, _) -> Dot(read_module env p, s)
    | Path.Papply(_, _)-> assert false

  let read_class_type env = function
    | Path.Pident id -> read_class_type_ident env id
    | Path.Pdot(p, s, _) -> Dot(read_module env p, s)
    | Path.Papply(_, _)-> assert false

  let read_type env = function
    | Path.Pident id -> read_type_ident env id
    | Path.Pdot(p, s, _) -> Dot(read_module env p, s)
    | Path.Papply(_, _)-> assert false

end

module Fragment = struct

  open Paths.Fragment.Resolved
  open Paths.Fragment

  let rec read_module = function
    | Longident.Lident s -> Dot(Resolved Root, s)
    | Longident.Ldot(p, s) -> Dot(signature_of_module (read_module p), s)
    | Longident.Lapply _ -> assert false

  let read_type = function
    | Longident.Lident s -> Dot(Resolved Root, s)
    | Longident.Ldot(p, s) -> Dot(signature_of_module (read_module p), s)
    | Longident.Lapply _ -> assert false

end
