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

open DocOckPaths
open DocOckTypes
open DocOckComponents

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

type 'a t =
  { equal : ('a -> 'a -> bool) option;
    hash : ('a -> int) option;
    lookup : 'a Unit.t -> string -> 'a option;
    fetch : 'a -> 'a Unit.t;
    tbl : ('a, 'a Sig.t) tbl; }

let create ?equal ?hash lookup fetch =
  let tbl = make_tbl equal hash 7 in
    {equal; hash; lookup; fetch; tbl}

type 'a local = ('a Identifier.signature, 'a Sig.t) tbl

let rec equal_ident equal id1 id2 =
  let open Identifier in
    match (id1 : 'a signature), (id2 : 'a signature) with
    | Root(r1, s1), Root(r2, s2) ->
        s1 = s2 && equal r1 r2
    | Module(id1, s1), Module(id2, s2) ->
        s1 = s2 && equal_ident equal id1 id2
    | Argument(id1, n1, s1), Argument(id2, n2, s2) ->
        n1 = n2 && s1 = s2 && equal_ident equal id1 id2
    | ModuleType(id1, s1), ModuleType(id2, s2) ->
        s1 = s2 && equal_ident equal id1 id2
    | _, _ -> false

let rec hash_ident hash id =
  let open Identifier in
    match (id : 'a signature) with
    | Root(r, s) ->
        Hashtbl.hash (1, hash r, s)
    | Module(id, s) ->
        Hashtbl.hash (2, hash_ident hash id, s)
    | Argument(id, n, s) ->
        Hashtbl.hash (3, hash_ident hash id, n, s)
    | ModuleType(id, s) ->
        Hashtbl.hash (4, hash_ident hash id, s)

let create_local equal hash =
  let equal =
    match equal with
    | None -> None
    | Some eq -> Some (equal_ident eq)
  in
  let hash =
    match hash with
    | None -> None
    | Some h -> Some (hash_ident h)
  in
    make_tbl equal hash 23

let add_local_module_identifier (local : 'a local) id sg =
  let open Identifier in
    local.add (signature_of_module id) sg

let add_local_module_type_identifier (local : 'a local) id sg =
  let open Identifier in
    local.add (signature_of_module_type id) sg

let local_module_identifier (local : 'a local option) id =
  let open Identifier in
    match local with
    | None -> None
    | Some local ->
        try
          Some (local.find (signature_of_module id))
        with Not_found -> None

let local_module_type_identifier (local : 'a local option) id =
  let open Identifier in
    match local with
    | None -> None
    | Some local ->
        try
          Some (local.find (signature_of_module_type id))
        with Not_found -> None

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
      DocOckPredef.core_types

let rec unit tbl base =
    try
      tbl.tbl.find base
    with Not_found ->
      let open Unit in
      let unt = tbl.fetch base in
      let local = create_local tbl.equal tbl.hash in
      let t =
        match unt.content with
        | Module items ->
            Sig.signature
              (fun items ->
                 Sig.add_documentation unt.doc
                   (signature_items tbl local unt items))
              items
        | Pack items ->
            Sig.signature
              (fun items ->
                 Sig.add_documentation unt.doc
                   (packed_items tbl local unt items))
              items
      in
        tbl.tbl.add base t;
        t

and signature_identifier tbl =
  let open Identifier in function
  | (Root(base, _) : 'a signature) -> unit tbl base
  | Module(id, name) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_module name parent
  | Argument(id, pos, name) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_argument pos parent
  | ModuleType(id, name) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_module_type name parent

and module_identifier tbl =
  let open Identifier in function
  | (Root(base, _) : 'a module_) -> unit tbl base
  | Module(id, name) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_module name parent
  | Argument(id, pos, name) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_argument pos parent

and module_type_identifier tbl =
  let open Identifier in function
  | (ModuleType(id, name) : 'a module_type) ->
      let parent = signature_identifier tbl id in
        Sig.lookup_module_type name parent

and datatype_identifier tbl =
  let open Identifier in function
  | (Type(id, name) : 'a Identifier.type_)->
      let parent = signature_identifier tbl id in
        Sig.lookup_datatype name parent
  | CoreType name -> List.assoc name core_types

and class_signature_identifier tbl =
  let open Identifier in function
    | (Class(id, name) | ClassType(id, name) : 'a path_class_type) ->
        let parent = signature_identifier tbl id in
          Sig.lookup_class_type name parent

and resolved_module_path tbl local u =
  let open Path.Resolved in function
  | Identifier (id : 'a Identifier.module_) -> begin
      match local_module_identifier local id with
      | Some sg -> sg
      | None ->  module_identifier tbl id
    end
  | Subst(sub, _) -> resolved_module_type_path tbl local u sub
  | SubstAlias(sub, _) -> resolved_module_path tbl local u sub
  | Module(p, name) ->
      let parent = resolved_module_path tbl local u p in
        Sig.lookup_module name parent
  | Apply(p, arg) ->
      let parent = resolved_module_path tbl local u p in
        Sig.lookup_apply (module_path tbl local u) arg parent

and resolved_module_type_path tbl local u =
  let open Path.Resolved in function
  | Identifier (id : 'a Identifier.module_type) -> begin
      match local_module_type_identifier local id with
      | Some sg -> sg
      | None -> module_type_identifier tbl id
    end
  | ModuleType(p, name) ->
      let parent = resolved_module_path tbl local u p in
        Sig.lookup_module_type name parent

and resolved_class_type_path tbl u =
  let open Path.Resolved in function
    | Identifier id -> class_signature_identifier tbl id
    | Class(p, name) | ClassType(p, name) ->
        let parent = resolved_module_path tbl None u p in
          Sig.lookup_class_type name parent

and module_path tbl local u =
  let open Path in function
  | Root s -> begin
      match tbl.lookup u s with
      | None -> Sig.unresolved
      | Some base -> unit tbl base
    end
  | Resolved r -> resolved_module_path tbl local u r
  | Dot(p, name) ->
      let parent = module_path tbl local u p in
        Sig.lookup_module name parent
  | Apply(p, arg) ->
      let parent = module_path tbl local u p in
        Sig.lookup_apply (module_path tbl local u) arg parent

and module_type_path tbl local u =
  let open Path in function
  | Resolved r -> resolved_module_type_path tbl local u r
  | Dot(p, name) ->
      let parent = module_path tbl local u p in
        Sig.lookup_module_type name parent

and class_signature_path tbl u =
  let open Path in function
    | Resolved p -> resolved_class_type_path tbl u p
    | Dot(p, name) ->
        let parent = module_path tbl None u p in
          Sig.lookup_class_type name parent

and class_signature_items tbl u =
  let open ClassSig in
  let open ClassSignature in function
    | InstanceVariable ivar :: rest ->
        let open InstanceVariable in
        let csig = class_signature_items tbl u rest in
        let csig = add_documentation ivar.doc csig in
        let name = Identifier.name ivar.id in
          add_element name Element.InstanceVariable csig
    | Method meth :: rest ->
        let open Method in
        let csig = class_signature_items tbl u rest in
        let csig = add_documentation meth.doc csig in
        let name = Identifier.name meth.id in
          add_element name Element.Method csig
    | Constraint _ :: rest ->
        class_signature_items tbl u rest
    | Inherit expr :: rest ->
        let csig = class_signature_items tbl u rest in
        let expr = class_type_expr tbl u expr in
          inherit_ expr csig
    | Comment comment :: rest ->
        let open Method in
        let csig = class_signature_items tbl u rest in
          add_comment comment csig
    | [] -> empty

and class_signature tbl u csig =
  let open ClassSignature in
    class_signature_items tbl u csig.items

and class_type_expr tbl u =
  let open ClassType in function
    | Constr(p, _) -> ClassSig.constr (class_signature_path tbl u) p
    | Signature csig -> ClassSig.signature (class_signature tbl u) csig

and class_decl tbl u =
  let open Class in function
    | ClassType expr -> class_type_expr tbl u expr
    | Arrow(_, _, decl) -> class_decl tbl u decl

and signature_items tbl local u =
  let open Sig in
  let open Signature in function
    | Module md :: rest ->
        let open Module in
        let name = Identifier.name md.id in
        let decl = module_decl tbl local u md.type_ in
        add_local_module_identifier local md.id decl;
        let sg = signature_items tbl local u rest in
        let sg = add_documentation md.doc sg in
          add_module name decl sg
    | ModuleType mty :: rest ->
        let open ModuleType in
        let name = Identifier.name mty.id in
        let expr =
          match mty.expr with
          | None -> abstract
          | Some expr -> module_type_expr tbl local u expr
        in
        add_local_module_type_identifier local mty.id expr;
        let sg = signature_items tbl local u rest in
        let sg = add_documentation mty.doc sg in
          add_module_type name expr sg
    | Type decl :: rest ->
        let open TypeDecl in
        let open Representation in
        let sg = signature_items tbl local u rest in
        let sg = add_documentation decl.doc sg in
        let name = Identifier.name decl.id in
        let decl = datatype decl in
          add_datatype name decl sg
    | TypExt ext :: rest ->
        let open Extension in
        let sg = signature_items tbl local u rest in
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
        let sg = signature_items tbl local u rest in
        let sg = add_documentation exn.doc sg in
        let name = Identifier.name exn.id in
          add_element name Element.Exception sg
    | Value v :: rest ->
        let open Value in
        let sg = signature_items tbl local u rest in
        let sg = add_documentation v.doc sg in
        let name = Identifier.name v.id in
          add_element name Element.Value sg
    | External ev :: rest ->
        let open External in
        let sg = signature_items tbl local u rest in
        let sg = add_documentation ev.doc sg in
        let name = Identifier.name ev.id in
          add_element name Element.Value sg
    | Class cl :: rest ->
        let open Class in
        let sg = signature_items tbl local u rest in
        let sg = add_documentation cl.doc sg in
        let name = Identifier.name cl.id in
        let expr = class_decl tbl u cl.type_ in
          add_class name expr sg
    | ClassType clty :: rest ->
        let open ClassType in
        let sg = signature_items tbl local u rest in
        let sg = add_documentation clty.doc sg in
        let name = Identifier.name clty.id in
        let expr = class_type_expr tbl u clty.expr in
          add_class_type name expr sg
    | Include expr :: rest ->
        let sg = signature_items tbl local u rest in
        let expr = module_type_expr tbl local u expr in
          include_ expr sg
    | Comment com :: rest ->
        let sg = signature_items tbl local u rest in
          add_comment com sg
    | [] -> empty

and module_type_expr tbl local u expr =
  let open Sig in
  let open ModuleType in
    match expr with
    | Path p -> path (module_type_path tbl (Some local) u) p
    | Signature sg -> signature (signature_items tbl local u) sg
    | Functor(Some(id, arg), res) ->
        let res = module_type_expr tbl local u res in
        let arg = module_type_expr tbl local u arg in
          functor_ tbl.equal tbl.hash id arg res
    | Functor(None, res) ->
        let res = module_type_expr tbl local u res in
          generative res
    | With(body, subs) ->
        let body = module_type_expr tbl local u body in
          List.fold_left
            (fun body sub ->
               match sub with
               | ModuleEq(frag, decl) ->
                   let eq = module_decl tbl local u decl in
                     with_module frag eq body
               | TypeEq _ -> body
               | ModuleSubst(frag, _) ->
                   with_module_subst frag body
               | TypeSubst(frag, _, _) ->
                   with_type_subst frag body)
            body subs
    | TypeOf decl -> module_decl tbl local u decl

and module_decl tbl local u decl =
  let open Sig in
  let open Module in
    match decl with
    | Alias p -> alias (module_path tbl (Some local) u) p
    | ModuleType expr -> module_type_expr tbl local u expr

and packed_items tbl local u =
  let open Sig in
  let open Unit.Packed in function
    | {id; path} :: rest ->
        let open Module in
        let name = Identifier.name id in
        let decl = alias (module_path tbl (Some local) u) path in
        add_local_module_identifier local id decl;
        let sg = packed_items tbl local u rest in
          add_module name decl sg
    | [] -> empty

(* Remove local parameter from exposed versions *)

let resolved_module_path tbl u p = resolved_module_path tbl None u p

let resolved_module_type_path tbl u p = resolved_module_type_path tbl None u p

let module_path tbl u p = module_path tbl None u p

(** TODO: One day we will be able to remove the unit component. *)
type 'a with_ =
  { unit: 'a Unit.t;
    base: 'a Sig.t;
    tbl: 'a t; }

let module_type_expr_with tbl unit expr =
  let local = create_local tbl.equal tbl.hash in
  let base = module_type_expr tbl local unit expr in
    { unit; base; tbl }

let module_type_path_with tbl unit path =
  let base = module_type_path tbl None unit path in
    { unit; base; tbl }

let rec resolved_signature_fragment wth =
  let open Fragment.Resolved in function
  | Root -> wth.base
  | Subst(sub, _) -> resolved_module_type_path wth.tbl wth.unit sub
  | SubstAlias(sub, _) -> resolved_module_path wth.tbl wth.unit sub
  | Module(p, name) ->
      let parent = resolved_signature_fragment wth p in
        Sig.lookup_module name parent

let rec resolved_signature_reference tbl =
  let open Reference.Resolved in function
  | Identifier (id : 'a Identifier.signature) ->
      signature_identifier tbl id
  | Module(p, name) ->
      let parent = resolved_signature_reference tbl p in
        Sig.lookup_module name parent
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

let base tbl s = tbl.lookup s
