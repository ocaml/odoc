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

open Model
open Paths
open Model.Lang
open Components

type ('a, 'b) tbl =
  { fresh: int -> ('a, 'b) tbl;
    find: 'a -> 'b;
    add: 'a -> 'b  -> unit; }

let make_tbl (type a) (equal : (a -> a -> bool) option)
           (hash : (a -> int) option) size =
  let make create find add =
    let rec fresh size =
      let t = create size in
      let find x = find t x in
      let add x y = add t x y in
        {fresh; find; add}
    in
      fresh size
  in
    match equal, hash with
    | None, None ->
        make (Hashtbl.create ?random:None) Hashtbl.find Hashtbl.add
    | _ ->
        let equal =
          match equal with
          | None -> (=)
          | Some eq -> eq
        in
        let hash =
          match hash with
          | None -> Hashtbl.hash
          | Some h -> h
        in
        let module Hash = struct
          type t = a
          let equal = equal
          let hash = hash
        end in
        let module Tbl = Hashtbl.Make(Hash) in
          make Tbl.create Tbl.find Tbl.add

type lookup_unit_result =
  | Forward_reference
  | Found of { root : Root.t; hidden : bool }
  | Not_found

type t =
  { equal : (Root.t -> Root.t -> bool) option;
    hash : (Root.t -> int) option;
    lookup_unit : string -> lookup_unit_result;
    lookup_page : string -> Root.t option;
    fetch_unit : Root.t -> Compilation_unit.t;
    fetch_page : Root.t -> Model.Lang.Page.t;
    tbl : (Root.t, Sig.t) tbl;
    page_tbl: (Root.t, Page.t) tbl; }

let create ?equal ?hash lookup_unit fetch_unit lookup_page fetch_page =
  let tbl = make_tbl equal hash 7 in
  let page_tbl = make_tbl equal hash 7 in
  {equal; hash; lookup_unit; fetch_unit; lookup_page; fetch_page; tbl; page_tbl}

type local =
  { t : t;
    local : (Identifier.signature, Sig.t) tbl option;
    base : Identifier.signature option; }

let create_local t base =
  let equal =
    match t.equal with
    | None -> None
    | Some _equal -> Some Identifier.equal
  in
  let hash =
    match t.hash with
    | None -> None
    | Some _hash -> Some Identifier.hash
  in
  let local =
    match base with
    | None -> None
    | Some _ -> Some (make_tbl equal hash 23)
  in
    { t; local; base; }

let add_local_module_identifier (local : local) id sg =
  let open Identifier in
    match local.local with
    | None -> ()
    | Some tbl -> tbl.add (signature_of_module id) sg

let add_local_module_type_identifier (local : local) id sg =
  let open Identifier in
    match local.local with
    | None -> ()
    | Some tbl -> tbl.add (signature_of_module_type id) sg

let add_local_modules (local : local) id mds =
  let open Identifier in
    match local.local with
    | None -> ()
    | Some tbl ->
        List.iter
          (fun (name, sg) -> tbl.add (Module(id, name)) sg)
          mds

let add_local_module_types (local : local) id mtys =
  let open Identifier in
    match local.local with
    | None -> ()
    | Some tbl ->
        List.iter
          (fun (name, sg) -> tbl.add (ModuleType(id, name)) sg)
          mtys

let equals_signature (type k) _eq
      (base : Identifier.signature) (id : k Identifier.t) =
  let open Identifier in
    match id with
    | Root _ as id ->
        Identifier.equal base id
    | Module _ as id ->
        Identifier.equal base id
    | Argument _ as id ->
        Identifier.equal base id
    | ModuleType _ as id ->
        Identifier.equal base id
    | Page _ -> false
    | Type _ -> false
    | CoreType _ -> false
    | Constructor _ -> false
    | Field _ -> false
    | Extension _ -> false
    | Exception _ -> false
    | CoreException _ -> false
    | Value _ -> false
    | Class _ -> false
    | ClassType _ -> false
    | Method _ -> false
    | InstanceVariable _ -> false
    | Label _ -> false

let rec is_parent_local : type k . _ -> _ -> k Identifier.t -> bool =
  fun eq base id ->
    let open Identifier in
      match id with
      | Root _  -> false
      | Page _ -> false
      | Module(parent, _) -> is_local eq base parent
      | Argument(parent, _, _) -> is_local eq base parent
      | ModuleType(parent, _) -> is_local eq base parent
      | Type(parent, _) -> is_local eq base parent
      | CoreType _ -> false
      | Constructor(parent, _) -> is_local eq base parent
      | Field(parent, _) -> is_local eq base parent
      | Extension(parent, _) -> is_local eq base parent
      | Exception(parent, _) -> is_local eq base parent
      | CoreException _ -> false
      | Value(parent, _) -> is_local eq base parent
      | Class(parent, _) -> is_local eq base parent
      | ClassType(parent, _) -> is_local eq base parent
      | Method(parent, _) -> is_local eq base parent
      | InstanceVariable(parent, _) -> is_local eq base parent
      | Label(parent, _) -> is_local eq base parent

and is_local : type k ._ -> _ -> k Identifier.t -> bool =
  fun eq base id ->
    is_parent_local eq base id
    || equals_signature eq base id

let is_local local id =
  match local.base with
  | None -> false
  | Some base ->
    let eq =
      match local.t.equal with
      | None -> (=)
      | Some eq -> eq
    in
    is_local eq base id

let local_module_identifier (local : local) id =
  let open Identifier in
    match local.local with
    | None -> Sig.unresolved
    | Some tbl ->
        try
          tbl.find (signature_of_module id)
        with Not_found -> Sig.unresolved

let local_module_type_identifier (local : local) id =
  let open Identifier in
    match local.local with
    | None -> Sig.unresolved
    | Some tbl ->
        try
          tbl.find (signature_of_module_type id)
        with Not_found -> Sig.unresolved

let datatype decl =
  let open TypeDecl in
  let open Representation in
  match decl.representation with
  | None -> Datatype.abstract
  | Some (Variant constructors) ->
      let open Constructor in
        let name = Identifier.name decl.id in
        let decl =
          Datatype.variant name
            (List.map (fun cstr -> Identifier.name cstr.id) constructors)
        in
        let decl =
          List.fold_right
            (fun cstr decl ->
               Datatype.add_documentation cstr.doc decl)
            constructors decl
        in
          decl
  | Some (Record fields) ->
      let open Field in
        let name = Identifier.name decl.id in
        let decl =
          Datatype.record name
            (List.map (fun field -> Identifier.name field.id) fields)
        in
        let decl =
          List.fold_right
            (fun field decl ->
               Datatype.add_documentation field.doc decl)
            fields decl
        in
          decl
  | Some Extensible -> Datatype.extensible

let core_types =
  let open TypeDecl in
    List.map
      (fun decl -> (Identifier.name decl.id, datatype decl))
      Predefined.core_types

let page tbl base =
  try
    tbl.page_tbl.find base
  with Not_found ->
    let page = tbl.fetch_page base in
    let t = Page.of_doc page.Model.Lang.Page.content in
    tbl.page_tbl.add base t;
    t

let page_identifier tbl : Identifier.page -> _ =
  let open Identifier in function
  | Page(base, _) -> page tbl base

let rec unit tbl base =
    try
      tbl.tbl.find base
    with Not_found ->
      let open Compilation_unit in
      let unt = tbl.fetch_unit base in
      let id = Identifier.signature_of_module unt.id in
      let local = create_local tbl (Some id) in
      let t =
        match unt.content with
        | Module items ->
            Sig.signature
              (fun items ->
                 Sig.add_documentation unt.doc
                   (signature_items local items))
              items
        | Pack items ->
            Sig.signature
              (fun items ->
                 Sig.add_documentation unt.doc
                   (packed_items local items))
              items
      in
      let t = Sig.set_hidden t unt.hidden in
        tbl.tbl.add base t;
        t

and signature_identifier tbl =
  let open Identifier in function
  | (Root(base, _) : signature) -> unit tbl base
  | Module(id, name) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_module name parent
  | Argument(id, pos, _) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_argument pos parent
  | ModuleType(id, name) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_module_type name parent

and module_identifier tbl =
  let open Identifier in function
  | (Root(base, _) : module_) -> unit tbl base
  | Module(id, name) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_module name parent
  | Argument(id, pos, _) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_argument pos parent

and module_type_identifier tbl =
  let open Identifier in function
  | (ModuleType(id, name) : module_type) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_module_type name parent

and datatype_identifier tbl =
  let open Identifier in function
  | (Type(id, name) : Identifier.type_)->
      let parent = signature_identifier tbl id in
        Sig.lookup_datatype name parent
  | CoreType name -> List.assoc name core_types

and class_signature_identifier tbl =
  let open Identifier in function
    | (Class(id, name) | ClassType(id, name) : path_class_type) ->
        let parent = signature_identifier tbl id in
          Sig.lookup_class_type name parent

and resolved_module_path local =
  let open Path.Resolved in function
  | Identifier (id : Identifier.module_) ->
      if is_local local id then local_module_identifier local id
      else module_identifier local.t id
  | Subst(sub, _) -> resolved_module_type_path local sub
  | SubstAlias(sub, _) -> resolved_module_path local sub
  | Hidden p -> resolved_module_path local p
  | Module(p, name) ->
      let parent = resolved_module_path local p in
        Sig.lookup_module name parent
  | Canonical (p, _) -> resolved_module_path local p
  | Apply(p, arg) ->
      let parent = resolved_module_path local p in
        Sig.lookup_apply (module_path local) arg parent

and resolved_module_type_path local =
  let open Path.Resolved in function
  | Identifier (id : Identifier.module_type) ->
      if is_local local id then local_module_type_identifier local id
      else module_type_identifier local.t id
  | ModuleType(p, name) ->
      let parent = resolved_module_path local p in
        Sig.lookup_module_type name parent

and resolved_class_type_path local =
  let open Path.Resolved in function
    | Identifier id -> class_signature_identifier local.t id
    | Class(p, name) | ClassType(p, name) ->
        let parent = resolved_module_path local p in
          Sig.lookup_class_type name parent

and module_path local =
  let open Path in function
  | Root s -> begin
      match local.t.lookup_unit s with
      | Not_found ->
        let sg = Sig.unresolved in
        Sig.set_hidden sg (Root.contains_double_underscore s)
      | Found {root;_} -> unit local.t root
      | Forward_reference ->
        let sg = Sig.abstract in
        Sig.set_hidden sg (Root.contains_double_underscore s)
    end
  | Forward s -> begin (* FIXME? *)
      match local.t.lookup_unit s with
      | Not_found ->
        let sg = Sig.unresolved in
        Sig.set_hidden sg (Root.contains_double_underscore s)
      | Found {root; _} -> unit local.t root
      | Forward_reference ->
        let sg = Sig.abstract in
        Sig.set_hidden sg (Root.contains_double_underscore s)
    end
  | Resolved r -> resolved_module_path local r
  | Dot(p, name) ->
      let parent = module_path local p in
        Sig.lookup_module name parent
  | Apply(p, arg) ->
      let parent = module_path local p in
        Sig.lookup_apply (module_path local) arg parent

and module_type_path local =
  let open Path in function
  | Resolved r -> resolved_module_type_path local r
  | Dot(p, name) ->
      let parent = module_path local p in
        Sig.lookup_module_type name parent

and class_signature_path local =
  let open Path in function
    | Resolved p -> resolved_class_type_path local p
    | Dot(p, name) ->
        let parent = module_path local p in
          Sig.lookup_class_type name parent

and class_signature_items local =
  let open ClassSig in
  let open ClassSignature in function
    | InstanceVariable ivar :: rest ->
        let open InstanceVariable in
        let csig = class_signature_items local rest in
        let csig = add_documentation ivar.doc csig in
        let name = Identifier.name ivar.id in
          add_element name Element.InstanceVariable csig
    | Method meth :: rest ->
        let open Method in
        let csig = class_signature_items local rest in
        let csig = add_documentation meth.doc csig in
        let name = Identifier.name meth.id in
          add_element name Element.Method csig
    | Constraint _ :: rest ->
        class_signature_items local rest
    | Inherit expr :: rest ->
        let csig = class_signature_items local rest in
        let expr = class_type_expr local expr in
          inherit_ expr csig
    | Comment comment :: rest ->
        let csig = class_signature_items local rest in
          add_comment comment csig
    | [] -> empty

and class_signature local csig =
  let open ClassSignature in
    class_signature_items local csig.items

and class_type_expr local =
  let open ClassType in function
    | Constr(p, _) -> ClassSig.constr (class_signature_path local) p
    | Signature csig -> ClassSig.signature (class_signature local) csig

and class_decl local =
  let open Class in function
    | ClassType expr -> class_type_expr local expr
    | Arrow(_, _, decl) -> class_decl local decl

and signature_items local =
  let open Sig in
  let open Signature in function
    | Module md :: rest ->
        let open Module in
        let name = Identifier.name md.id in
        let decl = module_decl local md.type_ in
        let decl = set_canonical decl md.canonical in
        let decl = set_hidden decl md.hidden in
        add_local_module_identifier local md.id decl;
        let sg = signature_items local rest in
        let sg = add_documentation md.doc sg in
          add_module name decl sg
    | ModuleType mty :: rest ->
        let open ModuleType in
        let name = Identifier.name mty.id in
        let expr =
          match mty.expr with
          | None -> abstract
          | Some expr -> module_type_expr local expr
        in
        add_local_module_type_identifier local mty.id expr;
        let sg = signature_items local rest in
        let sg = add_documentation mty.doc sg in
          add_module_type name expr sg
    | Type decl :: rest ->
        let open TypeDecl in
        let sg = signature_items local rest in
        let sg = add_documentation decl.doc sg in
        let name = Identifier.name decl.id in
        let decl = datatype decl in
          add_datatype name decl sg
    | TypExt ext :: rest ->
        let open Extension in
        let sg = signature_items local rest in
        let sg = add_documentation ext.doc sg in
          List.fold_right
            (fun cstr acc ->
               let open Constructor in
               let name = Identifier.name cstr.id in
               let acc = add_documentation cstr.doc acc in
                 add_element name Element.Extension acc)
            ext.constructors sg
    | Exception exn :: rest ->
        let open Exception in
        let sg = signature_items local rest in
        let sg = add_documentation exn.doc sg in
        let name = Identifier.name exn.id in
          add_element name Element.Exception sg
    | Value v :: rest ->
        let open Value in
        let sg = signature_items local rest in
        let sg = add_documentation v.doc sg in
        let name = Identifier.name v.id in
          add_element name Element.Value sg
    | External ev :: rest ->
        let open External in
        let sg = signature_items local rest in
        let sg = add_documentation ev.doc sg in
        let name = Identifier.name ev.id in
          add_element name Element.Value sg
    | Class cl :: rest ->
        let open Class in
        let sg = signature_items local rest in
        let sg = add_documentation cl.doc sg in
        let name = Identifier.name cl.id in
        let expr = class_decl local cl.type_ in
          add_class name expr sg
    | ClassType clty :: rest ->
        let open ClassType in
        let sg = signature_items local rest in
        let sg = add_documentation clty.doc sg in
        let name = Identifier.name clty.id in
        let expr = class_type_expr local clty.expr in
          add_class_type name expr sg
    | Include incl :: rest ->
        let open Include in
        let decl = module_decl local incl.decl in
        add_local_modules local incl.parent (modules decl);
        add_local_module_types local incl.parent (module_types decl);
        let sg = signature_items local rest in
        let sg = add_documentation incl.doc sg in
          include_ decl sg
    | Comment com :: rest ->
        let sg = signature_items local rest in
          add_comment com sg
    | [] -> empty

and module_type_expr local expr =
  let open Sig in
  let open ModuleType in
  let open FunctorArgument in
    match expr with
    | Path p -> path (module_type_path local) p
    | Signature sg -> signature (signature_items local) sg
    | Functor(Some{ id; expr = arg; _}, res) ->
        let res = module_type_expr local res in
        let arg = module_type_expr local arg in
          functor_ local.t.equal local.t.hash id arg res
    | Functor(None, res) ->
        let res = module_type_expr local res in
          generative res
    | With(body, subs) ->
        let body = module_type_expr local body in
          List.fold_left
            (fun body sub ->
               match sub with
               | ModuleEq(frag, decl) ->
                   let eq = module_decl local decl in
                     with_module frag eq body
               | TypeEq _ -> body
               | ModuleSubst(frag, _) ->
                   with_module_subst frag body
               | TypeSubst(frag, _) ->
                   with_type_subst frag body)
            body subs
    | TypeOf decl -> module_decl local decl

and module_decl local decl =
  let open Sig in
  let open Module in
    match decl with
    | Alias p -> alias (module_path local) p
    | ModuleType expr -> module_type_expr local expr

and packed_items local =
  let open Sig in
  let open Compilation_unit.Packed in function
    | {id; path} :: rest ->
        let name = Identifier.name id in
        let decl = alias (module_path local) path in
        add_local_module_identifier local id decl;
        let sg = packed_items local rest in
          add_module name decl sg
    | [] -> empty

(* Remove local parameter from exposed versions *)

let resolved_module_path tbl p =
  let local = create_local tbl None in
    resolved_module_path local p

let resolved_module_type_path tbl p =
  let local = create_local tbl None in
    resolved_module_type_path local p

let resolved_class_type_path tbl p =
  let local = create_local tbl None in
    resolved_class_type_path local p

let module_path tbl p =
  let local = create_local tbl None in
    module_path local p

type with_ =
  { base: Sig.t;
    tbl: t; }

let module_type_expr_with tbl id expr =
  let local = create_local tbl (Some id) in
  let base = module_type_expr local expr in
    { base; tbl }

let module_type_path_with tbl path =
  let local = create_local tbl None in
  let base = module_type_path local path in
    { base; tbl }

let rec resolved_signature_fragment wth =
  let open Fragment.Resolved in function
  | Root -> wth.base
  | Subst(sub, _) -> resolved_module_type_path wth.tbl sub
  | SubstAlias(sub, _) -> resolved_module_path wth.tbl sub
  | Module(p, name) ->
      let parent = resolved_signature_fragment wth p in
        Sig.lookup_module name parent

let rec resolved_signature_reference tbl =
  let open Reference.Resolved in function
  | Identifier (id : Identifier.signature) ->
      signature_identifier tbl id
  | SubstAlias(sub, _) ->
      resolved_module_path tbl sub
  | Module(p, name) ->
      let parent = resolved_signature_reference tbl p in
        Sig.lookup_module name parent
  | Canonical (p, _) ->
    resolved_signature_reference tbl (signature_of_module p)
  | ModuleType(p, name) ->
      let parent = resolved_signature_reference tbl p in
        Sig.lookup_module_type name parent

and resolved_class_signature_reference tbl =
  let open Reference.Resolved in function
    | Identifier id -> class_signature_identifier tbl id
    | Class(p, name) | ClassType(p, name) ->
        let parent = resolved_signature_reference tbl p in
          Sig.lookup_class_type name parent

and resolved_datatype_reference tbl =
  let open Reference.Resolved in function
    | Identifier id -> datatype_identifier tbl id
    | Type(p, name) ->
        let parent = resolved_signature_reference tbl p in
          Sig.lookup_datatype name parent

and resolved_page_reference tbl : Reference.Resolved.page -> _ =
  let open Reference.Resolved in function
    | Identifier id -> page_identifier tbl id

let base tbl s = tbl.lookup_unit s

let page_base tbl s = tbl.lookup_page s
