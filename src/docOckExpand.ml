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

type 'a module_expansion =
  | Signature of 'a Signature.t
  | Functor of ('a Identifier.module_ *
                'a ModuleType.expr) option list *
               'a Signature.t

let subst_signature sub = function
  | None -> None
  | Some sg -> Some (DocOckSubst.signature sub sg)

let subst_arg sub arg =
  match arg with
  | None -> None
  | Some (id, expr) ->
      let id' = DocOckSubst.identifier_module sub id in
      let expr' = DocOckSubst.module_type_expr sub expr in
        Some (id', expr')

let subst_expansion sub = function
  | None -> None
  | Some (Signature sg) ->
      let sg' = DocOckSubst.signature sub sg in
        Some (Signature sg')
  | Some (Functor(args, sg)) ->
      let args' = List.map (subst_arg sub) args in
      let expr' = DocOckSubst.signature sub sg in
        Some (Functor(args', expr'))

let rec filter_opt = function
  | [] -> []
  | None :: rest -> filter_opt rest
  | Some x ::rest -> x :: filter_opt rest

let find_module name ex =
  let rec loop name items =
    let open Signature in
    let open Module in
      match items with
      | [] -> raise Not_found
      | Module md :: _ when Identifier.name md.id = name -> md
      | _ :: rest -> loop name rest
  in
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> loop name items
    | Some (Functor _) -> raise Not_found

let find_argument pos ex =
  let rec loop pos args =
    let open Signature in
      match args with
      | [] -> raise Not_found
      | arg :: _  when pos = 0 -> begin
          match arg with
          | None -> raise Not_found
          | Some arg -> arg
        end
      | _ :: rest -> loop (pos - 1) rest
  in
    match ex with
    | None -> raise Not_found
    | Some (Signature _) -> raise Not_found
    | Some (Functor(args, _)) -> loop pos args

let find_module_type name ex =
  let rec loop name items =
    let open Signature in
    let open ModuleType in
      match items with
      | [] -> raise Not_found
      | ModuleType mty :: _ when Identifier.name mty.id = name -> mty
      | _ :: rest -> loop name rest
  in
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> loop name items
    | Some (Functor _) -> raise Not_found

type 'a t =
  { equal: 'a -> 'a -> bool;
    hash: 'a -> int;
    fetch: 'a -> 'a Unit.t; }

let build_expander (type a) ?equal ?hash (fetch : a -> a Unit.t) =
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
  let fetch =
    let module Hash = struct
      type t = a
      let equal = equal
      let hash = hash
    end in
    let module Tbl = Hashtbl.Make(Hash) in
    let tbl = Tbl.create 13 in
      fun root ->
        try
          Tbl.find tbl root
        with Not_found ->
          let unit = fetch root in
            Tbl.add tbl root unit;
            unit
  in
    { equal; hash; fetch }

let rec expand_signature_identifier t id =
  let open Identifier in
  match id with
  | Root(root, name) ->
      let open Unit in
      let unit = t.fetch root in
      let id = signature_of_module unit.id in
      let sg = expand_unit_content t id unit.content in
      let ex =
        match sg with
        | None -> None
        | Some sg -> Some (Signature sg)
      in
        ex
  | Module(parent, name) ->
      let open Module in
      let ex = expand_signature_identifier t parent in
      let md = find_module name ex in
        expand_module t md
  | Argument(parent, pos, name) ->
      let ex = expand_signature_identifier t parent in
      let id, expr = find_argument pos ex in
      let id = signature_of_module id in
        expand_module_type_expr t id expr
  | ModuleType(parent, name) ->
      let open ModuleType in
      let ex = expand_signature_identifier t parent in
      let mty = find_module_type name ex in
        expand_module_type t mty

and expand_module_identifier t (id : 'a Identifier.module_) =
  let open Identifier in
  match id with
  | Root(root, name) ->
      let open Unit in
      let unit = t.fetch root in
      let sg = expand_unit t unit in
      let ex =
        match sg with
        | None -> None
        | Some sg -> Some (Signature sg)
      in
        unit.id, unit.doc, ex
  | Module(parent, name) ->
      let open Module in
      let ex = expand_signature_identifier t parent in
      let md = find_module name ex in
        md.id, md.doc, expand_module t md
  | Argument(parent, pos, name) ->
      let ex = expand_signature_identifier t parent in
      let (id, expr) = find_argument pos ex in
      let doc = DocOckAttrs.empty in
        id, doc, expand_module_type_expr t (signature_of_module id) expr

and expand_module_type_identifier t (id : 'a Identifier.module_type) =
  let open Identifier in
  match id with
  | ModuleType(parent, name) ->
      let open ModuleType in
      let ex = expand_signature_identifier t parent in
      let mty = find_module_type name ex in
        mty.id, mty.doc, expand_module_type t mty

and expand_module_resolved_path ({equal = eq} as t) p =
  let open Path.Resolved in
  match p with
  | Identifier id -> expand_module_identifier t id
  | Subst(_, p) -> expand_module_resolved_path t p
  | SubstAlias(_, p) -> expand_module_resolved_path t p
  | Module(parent, name) ->
      let open Module in
      let id, _, ex = expand_module_resolved_path t parent in
      let md = find_module name ex in
      let sub = DocOckSubst.prefix ~equal:eq id in
      let md' = DocOckSubst.module_ sub md in
        md'.id, md'.doc, expand_module t md'
  | Apply _ -> raise Not_found (* TODO support functor application *)

and expand_module_type_resolved_path ({equal = eq} as t)
                                     (p : 'a Path.Resolved.module_type) =
  let open Path.Resolved in
  match p with
  | Identifier id -> expand_module_type_identifier t id
  | ModuleType(parent, name) ->
      let open ModuleType in
      let id, _, ex = expand_module_resolved_path t parent in
      let mty = find_module_type name ex in
      let sub = DocOckSubst.prefix ~equal:eq id in
      let mty' = DocOckSubst.module_type sub mty in
        mty'.id, mty'.doc, expand_module_type t mty'

and expand_module_decl ({equal} as t) dest decl =
  let open Module in
    match decl with
    | Alias (Path.Resolved p) -> begin (* TODO Should have strengthening *)
        match expand_module_resolved_path t p with
        | src, _, ex ->
          let src = Identifier.signature_of_module src in
          let sub = DocOckSubst.rename_signature ~equal src dest in
          subst_expansion sub ex
        | exception Not_found -> None (* TODO: Should be an error *)
      end
    | Alias _ -> None
    | ModuleType expr -> expand_module_type_expr t dest expr

and expand_module_type_expr ({equal} as t) dest expr =
  let open ModuleType in
    match expr with
    | Path (Path.Resolved p) -> begin
        match expand_module_type_resolved_path t p with
        | src, _, ex ->
          let src = Identifier.signature_of_module_type src in
          let sub = DocOckSubst.rename_signature ~equal src dest in
            subst_expansion sub ex
        | exception Not_found -> None (* TODO: Should be an error *)
      end
    | Path _ -> None
    | Signature sg -> Some (Signature sg)
    | Functor(arg, expr) -> begin
        match expand_module_type_expr t dest expr with
        | None -> None
        | Some (Signature sg) -> Some(Functor([arg], sg))
        | Some (Functor(args, sg)) -> Some(Functor(arg :: args, sg))
      end
    | With _ -> None (* TODO support with substitution *)
    | TypeOf decl -> expand_module_decl t dest decl

and expand_unit_content ({equal; hash} as t) dest content =
  let open Unit in
    match content with
    | Pack items ->
        let open Packed in
        let rec loop ids mds = function
          | [] ->
            let open Signature in
            let sg = List.rev_map (fun md -> Module md) mds in
            ids, Some sg
          | item :: rest ->
              match item.path with
              | Path.Resolved p -> begin
                  match expand_module_resolved_path t p with
                  | src, doc, ex -> begin
                    match ex with
                    | None -> [], None
                    | Some (Functor _) ->
                        [], None (* TODO should be an error *)
                    | Some (Signature sg) ->
                        let open Module in
                        let id = item.id in
                        let type_ = ModuleType (ModuleType.Signature sg) in
                        let md = {id; doc; type_} in
                        loop ((src, item.id) :: ids) (md :: mds) rest
                    end
                  | exception Not_found -> [], None (* TODO: Should be an error *)
                end
              | _ -> [], None
        in
        let ids, sg = loop [] [] items in
        let sub = DocOckSubst.pack ~equal ~hash ids in
          subst_signature sub sg
    | Module sg -> Some sg

and expand_module t md =
  let open Module in
  let id = Identifier.signature_of_module md.id in
  expand_module_decl t id md.type_

and expand_module_type t mty =
  let open ModuleType in
  match mty.expr with
  | Some expr ->
      let id = Identifier.signature_of_module_type mty.id in
        expand_module_type_expr t id expr
  | None -> None

and expand_unit t unit =
  let open Unit in
  let id = Identifier.signature_of_module unit.id in
  expand_unit_content t id unit.content

let expand_include t incl =
  let open Include in
  match expand_module_type_expr t incl.parent incl.expr with
  | None -> None
  | Some (Signature sg) -> Some sg
  | Some (Functor _) -> None
