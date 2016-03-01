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

type 'a partial_expansion =
  | Signature of 'a Signature.t
  | Functor of ('a Identifier.module_ *
                'a ModuleType.expr) option *
               'a Identifier.signature * int *
               'a ModuleType.expr

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
  | Some (Functor(arg, id, offset, expr)) ->
      let arg' = subst_arg sub arg in
      let id', offset' =
        DocOckSubst.offset_identifier_signature sub (id, offset)
      in
      let expr' = DocOckSubst.module_type_expr sub expr in
        Some (Functor(arg', id', offset', expr'))

let map_module name ex f =
  let rec loop name items f acc =
    let open Signature in
    let open Module in
      match items with
      | [] -> raise Not_found
      | Module md :: rest when Identifier.name md.id = name ->
        let md' = f md in
        List.rev_append acc ((Module md') :: rest)
      | item :: rest -> loop name rest f (item :: acc)
  in
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> Some (Signature (loop name items f []))
    | Some (Functor _) -> raise Not_found

let map_type name ex f =
  let rec loop name items f acc =
    let open Signature in
    let open TypeDecl in
      match items with
      | [] -> raise Not_found
      | Type decl :: rest when Identifier.name decl.id = name ->
        let decl' = f decl in
        List.rev_append acc ((Type decl') :: rest)
      | item :: rest -> loop name rest f (item :: acc)
  in
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> Some (Signature (loop name items f []))
    | Some (Functor _) -> raise Not_found

let add_module_with subst md =
  let open Module in
  let open ModuleType in
  let type_ =
    match md.type_ with
    | Alias _ as decl ->
        ModuleType(With(TypeOf decl, [subst]))
    | ModuleType(With(expr, substs)) ->
        ModuleType(With(expr, substs @ [subst]))
    | ModuleType expr ->
        ModuleType(With(expr, [subst]))
  in
    { md with type_ }

let refine_type ex (frag : 'a Fragment.type_) equation =
  let open Fragment in
  match frag with
  | Dot _ -> None
  | Resolved frag ->
    let open Resolved in
    let name, rest = split frag in
      match rest with
      | None -> begin
          try
            map_type name ex
              (fun decl -> TypeDecl.{ decl with equation })
          with Not_found -> None (* TODO should be an error *)
        end
      | Some frag -> begin
          let subst = ModuleType.TypeEq(Resolved frag, equation) in
          try
            map_module name ex (add_module_with subst)
          with Not_found -> None (* TODO should be an error *)
        end

let refine_module ex (frag : 'a Fragment.module_) equation =
  let open Fragment in
  match frag with
  | Dot _ -> None
  | Resolved frag ->
    let open Resolved in
    let name, rest = split frag in
      match rest with
      | None -> begin
          try
            map_module name ex
              (fun md -> Module.{ md with type_ = equation})
              (* TODO Fix this to not produce an alias (needs strengthening)
                      or fix OCaml to do the correct thing. *)
          with Not_found -> None (* TODO should be an error *)
        end
      | Some frag -> begin
          let subst = ModuleType.ModuleEq(Resolved frag, equation) in
          try
            map_module name ex (add_module_with subst)
          with Not_found -> None (* TODO should be an error *)
        end

type 'a intermediate_module_expansion =
  'a Identifier.module_ * 'a Documentation.t * 'a partial_expansion option

type 'a intermediate_module_type_expansion =
  'a Identifier.module_type * 'a Documentation.t * 'a partial_expansion option

type 'a t =
  { equal: 'a -> 'a -> bool;
    hash: 'a -> int;
    expand_root: root:'a -> 'a -> 'a intermediate_module_expansion;
    expand_module_identifier: root:'a -> 'a Identifier.module_ ->
                              'a intermediate_module_expansion;
    expand_module_type_identifier: root:'a -> 'a Identifier.module_type ->
                                   'a intermediate_module_type_expansion;
    expand_signature_identifier: root:'a -> 'a Identifier.signature ->
                                 'a partial_expansion option;
    expand_module_resolved_path: root:'a -> 'a Path.Resolved.module_ ->
                                 'a intermediate_module_expansion;
    expand_module_type_resolved_path: root:'a ->
                                      'a Path.Resolved.module_type ->
                                      'a intermediate_module_type_expansion; }

let rec expand_module_decl ({equal} as t) root dest offset decl =
  let open Module in
    match decl with
    | Alias (Path.Resolved p) -> begin (* TODO Should have strengthening *)
        match t.expand_module_resolved_path ~root p with
        | src, _, ex ->
          let src = Identifier.signature_of_module src in
          let sub = DocOckSubst.rename_signature ~equal src dest offset in
          subst_expansion sub ex
        | exception Not_found -> None (* TODO: Should be an error *)
      end
    | Alias _ -> None
    | ModuleType expr -> expand_module_type_expr t root dest offset expr

and expand_module_type_expr ({equal} as t) root dest offset expr =
  let open ModuleType in
    match expr with
    | Path (Path.Resolved p) -> begin
        match t.expand_module_type_resolved_path ~root p with
        | src, _, ex ->
          let src = Identifier.signature_of_module_type src in
          let sub = DocOckSubst.rename_signature ~equal src dest offset in
            subst_expansion sub ex
        | exception Not_found -> None (* TODO: Should be an error *)
      end
    | Path _ -> None
    | Signature sg -> Some (Signature sg)
    | Functor(arg, expr) -> Some (Functor(arg, dest, (offset + 1), expr))
    | With(expr, substs) ->
        let ex = expand_module_type_expr t root dest offset expr in
          List.fold_left
            (fun ex subst ->
               match subst with
               | TypeEq(frag, eq) -> refine_type ex frag eq
               | ModuleEq(frag, eq) -> refine_module ex frag eq
               | TypeSubst _ -> ex (* TODO perform substitution *)
               | ModuleSubst _ -> ex (* TODO perform substitution *))
            ex substs
    | TypeOf decl ->
        expand_module_decl t root dest offset decl (* TODO perform weakening *)

let expand_module t root md =
  let open Module in
  let id = Identifier.signature_of_module md.id in
  expand_module_decl t root id 0 md.type_

let expand_module_type t root mty =
  let open ModuleType in
  match mty.expr with
  | Some expr ->
      let id = Identifier.signature_of_module_type mty.id in
        expand_module_type_expr t root id 0 expr
  | None -> None

let expand_include t root incl =
  let open Include in
  match expand_module_decl t root incl.parent 0 incl.decl with
  | None -> None
  | Some (Signature sg) -> Some sg
  | Some (Functor _) -> None (* TODO: Should be an error *)

let expand_argument t root (id, expr) =
  let id = Identifier.signature_of_module id in
  expand_module_type_expr t root id 0 expr

let find_module t root name ex =
  let rec inner_loop name items =
    let open Signature in
    let open Module in
      match items with
      | [] -> raise Not_found
      | Module md :: _ when Identifier.name md.id = name -> md
      | Include incl :: rest -> begin
          match expand_include t root incl with
          | None -> inner_loop name rest
          | Some sg -> inner_loop name (sg @ rest)
        end
      | _ :: rest -> inner_loop name rest
  in
  let rec loop t root name ex =
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> inner_loop name items
    | Some (Functor(_, dest, offset, expr)) ->
        loop t root name (expand_module_type_expr t root dest offset expr)
  in
    loop t root name ex

let find_argument t root pos ex =
  let rec loop t root pos ex =
    match ex with
    | None -> raise Not_found
    | Some (Signature _) -> raise Not_found
    | Some (Functor(None, _, _, _)) when pos = 1 -> raise Not_found
    | Some (Functor(Some arg, _, _, _)) when pos = 1 -> arg
    | Some (Functor(_, dest, offset, expr)) ->
        loop t root (pos - 1) (expand_module_type_expr t root dest offset expr)
  in
    loop t root pos ex

let find_module_type t root name ex =
  let rec inner_loop name items =
    let open Signature in
    let open ModuleType in
      match items with
      | [] -> raise Not_found
      | ModuleType mty :: _ when Identifier.name mty.id = name -> mty
      | Include incl :: rest -> begin
          match expand_include t root incl with
          | None -> inner_loop name rest
          | Some sg -> inner_loop name (sg @ rest)
        end
      | _ :: rest -> inner_loop name rest
  in
  let rec loop t root name ex =
    match ex with
    | None -> raise Not_found
    | Some (Signature items) -> inner_loop name items
    | Some (Functor(_, dest, offset, expr)) ->
        loop t root name (expand_module_type_expr t root dest offset expr)
  in
    loop t root name ex

let expand_signature_identifier' t root (id : 'a Identifier.signature) =
  let open Identifier in
  match id with
  | Root(root', name) ->
      let open Unit in
      let _, _, ex = t.expand_root ~root root' in
        ex
  | Module(parent, name) ->
      let open Module in
      let ex = t.expand_signature_identifier root parent in
      let md = find_module t root name ex in
        expand_module t root md
  | Argument(parent, pos, name) ->
      let ex = t.expand_signature_identifier ~root parent in
      let arg = find_argument t root pos ex in
        expand_argument t root arg
  | ModuleType(parent, name) ->
      let open ModuleType in
      let ex = t.expand_signature_identifier ~root parent in
      let mty = find_module_type t root name ex in
        expand_module_type t root mty

and expand_module_identifier' t root (id : 'a Identifier.module_) =
  let open Identifier in
  match id with
  | Root(root', name) -> t.expand_root ~root root'
  | Module(parent, name) ->
      let open Module in
      let ex = t.expand_signature_identifier ~root parent in
      let md = find_module t root name ex in
        md.id, md.doc, expand_module t root md
  | Argument(parent, pos, name) ->
      let ex = t.expand_signature_identifier ~root parent in
      let (id, _) as arg = find_argument t root pos ex in
      let doc = DocOckAttrs.empty in
        id, doc, expand_argument t root arg

and expand_module_type_identifier' t root (id : 'a Identifier.module_type) =
  let open Identifier in
  match id with
  | ModuleType(parent, name) ->
      let open ModuleType in
      let ex = t.expand_signature_identifier ~root parent in
      let mty = find_module_type t root name ex in
        mty.id, mty.doc, expand_module_type t root mty

and expand_module_resolved_path' ({equal = eq} as t) root p =
  let open Path.Resolved in
  match p with
  | Identifier id -> t.expand_module_identifier ~root id
  | Subst(_, p) -> t.expand_module_resolved_path ~root p
  | SubstAlias(_, p) -> t.expand_module_resolved_path ~root p
  | Module(parent, name) ->
      let open Module in
      let id, _, ex = t.expand_module_resolved_path ~root parent in
      let md = find_module t root name ex in
      let sub = DocOckSubst.prefix ~equal:eq id in
      let md' = DocOckSubst.module_ sub md in
        md'.id, md'.doc, expand_module t root md'
  | Apply _ -> raise Not_found (* TODO support functor application *)

and expand_module_type_resolved_path' ({equal = eq} as t) root
                                     (p : 'a Path.Resolved.module_type) =
  let open Path.Resolved in
  match p with
  | Identifier id -> t.expand_module_type_identifier ~root id
  | ModuleType(parent, name) ->
      let open ModuleType in
      let id, _, ex = t.expand_module_resolved_path ~root parent in
      let mty = find_module_type t root name ex in
      let sub = DocOckSubst.prefix ~equal:eq id in
      let mty' = DocOckSubst.module_type sub mty in
        mty'.id, mty'.doc, expand_module_type t root mty'

and expand_unit ({equal; hash} as t) root unit =
  let open Unit in
    match unit.content with
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
                  match t.expand_module_resolved_path ~root p with
                  | src, doc, ex -> begin
                    match ex with
                    | None -> [], None
                    | Some (Functor _) ->
                        [], None (* TODO should be an error *)
                    | Some (Signature sg) ->
                        let open Module in
                        let id = item.id in
                        let type_ = ModuleType (ModuleType.Signature sg) in
                        let md = {id; doc; type_; expansion = Some sg} in
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


let build_expander (type a) ?equal ?hash (fetch : root:a -> a -> a Unit.t) =
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
  let module RootHash = struct
    type t = a * a
    let equal (a1, b1) (a2, b2) = equal a1 a2 && equal b1 b2
    let hash (a, b) = Hashtbl.hash (hash a, hash b)
  end in
  let module RootTbl = Hashtbl.Make(RootHash) in
  let expand_root_tbl = RootTbl.create 13 in
  let module IdentifierHash = struct
    type t = a * a Identifier.any
    let equal (root1, id1) (root2, id2) =
      equal root1 root2 && Identifier.equal ~equal id1 id2
    let hash (root, id) =
      Hashtbl.hash (hash root, Identifier.hash ~hash id)
  end in
  let module IdentifierTbl = Hashtbl.Make(IdentifierHash) in
  let expand_module_identifier_tbl = IdentifierTbl.create 13 in
  let expand_module_type_identifier_tbl = IdentifierTbl.create 13 in
  let expand_signature_identifier_tbl = IdentifierTbl.create 13 in
  let module PathHash = struct
    type t = a * a Path.Resolved.any
    let equal (root1, p1) (root2, p2) =
      equal root1 root2 && Path.Resolved.equal ~equal p1 p2
    let hash (root, p) =
      Hashtbl.hash (hash root, Path.Resolved.hash ~hash)
  end in
  let module PathTbl = Hashtbl.Make(PathHash) in
  let expand_module_resolved_path_tbl = PathTbl.create 13 in
  let expand_module_type_resolved_path_tbl = PathTbl.create 13 in
  let rec expand_root ~root root' =
    let key = (root, root') in
    try
      RootTbl.find expand_root_tbl key
    with Not_found ->
      let open Unit in
      let unit = fetch ~root root' in
      let sg = expand_unit t root unit in
      let ex =
        match sg with
        | None -> None
        | Some sg -> Some (Signature sg)
      in
      let res = (unit.id, unit.doc, ex) in
      RootTbl.add expand_root_tbl key res;
      res
  and expand_module_identifier ~root id =
    let key = (root, Identifier.any id) in
    try
      IdentifierTbl.find expand_module_identifier_tbl key
    with Not_found ->
      let res = expand_module_identifier' t root id in
      IdentifierTbl.add expand_module_identifier_tbl key res;
      res
  and expand_module_type_identifier ~root id =
    let key = (root, Identifier.any id) in
    try
      IdentifierTbl.find expand_module_type_identifier_tbl key
    with Not_found ->
      let res = expand_module_type_identifier' t root id in
      IdentifierTbl.add expand_module_type_identifier_tbl key res;
      res
  and expand_signature_identifier ~root id =
    let key = (root, Identifier.any id) in
    try
      IdentifierTbl.find expand_signature_identifier_tbl key
    with Not_found ->
      let res = expand_signature_identifier' t root id in
      IdentifierTbl.add expand_signature_identifier_tbl key res;
      res
  and expand_module_resolved_path ~root p =
    let key = (root, Path.Resolved.any p) in
    try
      PathTbl.find expand_module_resolved_path_tbl key
    with Not_found ->
      let res = expand_module_resolved_path' t root p in
      PathTbl.add expand_module_resolved_path_tbl key res;
      res
  and expand_module_type_resolved_path ~root p =
    let key = (root, Path.Resolved.any p) in
    try
      PathTbl.find expand_module_type_resolved_path_tbl key
    with Not_found ->
      let res = expand_module_type_resolved_path' t root p in
      PathTbl.add expand_module_type_resolved_path_tbl key res;
      res
  and t =
    { equal; hash;
      expand_root;
      expand_module_identifier;
      expand_module_type_identifier;
      expand_signature_identifier;
      expand_module_resolved_path;
      expand_module_type_resolved_path; }
  in
    t

type 'a module_expansion =
  | Signature of 'a Signature.t
  | Functor of ('a Identifier.module_ *
                'a ModuleType.expr) option list *
               'a Signature.t

let rec force_expansion t root (ex : 'a partial_expansion option) =
  match ex with
  | None -> None
  | Some (Signature sg) -> Some (Signature sg)
  | Some (Functor(arg, dest, offset, expr)) ->
      let ex = expand_module_type_expr t root dest offset expr in
        match force_expansion t root ex with
        | None -> None
        | Some (Signature sg) -> Some(Functor([arg], sg))
        | Some (Functor(args, sg)) -> Some(Functor(arg :: args, sg))

let expand_module t md =
  let open Module in
  let root = Identifier.module_root md.id in
  force_expansion t root (expand_module t root md)

let expand_module_type t mty =
  let open ModuleType in
  let root = Identifier.module_type_root mty.id in
  force_expansion t root (expand_module_type t root mty)

let expand_include t incl =
  let open Include in
  let root = Identifier.signature_root incl.parent in
  expand_include t root incl

let expand_unit t unit =
  let open Unit in
  let root = Identifier.module_root unit.id in
  expand_unit t root unit

let expand_argument t ((id, _) as arg) =
  let root = Identifier.module_root id in
  force_expansion t root (expand_argument t root arg)
