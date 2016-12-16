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
  | Functor of 'a FunctorArgument.t option *
               'a Identifier.signature * int *
               'a ModuleType.expr

let subst_signature sub = function
  | None -> None
  | Some sg -> Some (DocOckSubst.signature sub sg)

let subst_arg sub arg =
  match arg with
  | None -> None
  | Some {FunctorArgument. id; expr; expansion} ->
      let id' = DocOckSubst.identifier_module sub id in
      let expr' = DocOckSubst.module_type_expr sub expr in
      let expansion' =
        DocOckMaps.option_map (DocOckSubst.module_expansion sub) expansion
      in
        Some {FunctorArgument. id = id'; expr = expr'; expansion = expansion'}

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
      | [] ->
        List.rev acc
        (* raise Not_found *)
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
      | [] ->
        List.rev acc
        (* raise Not_found *)
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

type 'a expander =
  { equal: 'a -> 'a -> bool;
    hash: 'a -> int;
    expand_root: root:'a -> 'a -> 'a intermediate_module_expansion;
    expand_forward_ref : root:'a -> string -> 'a intermediate_module_expansion;
    expand_module_identifier: root:'a -> 'a Identifier.module_ ->
                              'a intermediate_module_expansion;
    expand_module_type_identifier: root:'a -> 'a Identifier.module_type ->
                                   'a intermediate_module_type_expansion;
    expand_signature_identifier: root:'a -> 'a Identifier.signature ->
                                 'a partial_expansion option;
    expand_module_resolved_path: root:'a -> 'a Path.Resolved.module_ ->
                                 'a intermediate_module_expansion;
    expand_module_path: root:'a -> 'a Path.module_ ->
                                 'a intermediate_module_expansion;
    expand_module_type_resolved_path: root:'a ->
                                      'a Path.Resolved.module_type ->
                                      'a intermediate_module_type_expansion; }

let add_doc_to_expansion_opt doc :
  'a partial_expansion option -> 'a partial_expansion option = function
  | Some (Signature sg) ->
      let doc = Signature.Comment (Documentation.Documentation doc) in
      Some (Signature (doc :: sg))
  | otherwise -> otherwise (* FIXME: handle functors? *)

let rec expand_module_decl ({equal} as t) root dest offset decl =
  let open Module in
    match decl with
    | Alias (Path.Resolved p) -> begin (* TODO Should have strengthening *)
        match t.expand_module_resolved_path ~root p with
        | src, doc, ex ->
          let src = Identifier.signature_of_module src in
          let sub = DocOckSubst.rename_signature ~equal src dest offset in
          let ex  = add_doc_to_expansion_opt doc ex in
          subst_expansion sub ex
        | exception Not_found -> None (* TODO: Should be an error *)
      end
    | Alias p -> begin
        match t.expand_module_path ~root p with
        | src, doc, ex ->
          let src = Identifier.signature_of_module src in
          let sub = DocOckSubst.rename_signature ~equal src dest offset in
          let ex = add_doc_to_expansion_opt doc ex in
          subst_expansion sub ex
        | exception Not_found -> None (* TODO: Should be an error *)
      end
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
  | None -> Some (Signature [])

let expand_include t root incl =
  let open Include in
    if incl.expansion.resolved then Some incl.expansion.content
    else begin
      match expand_module_decl t root incl.parent 0 incl.decl with
      | None -> Some incl.expansion.content
      | Some (Signature sg) -> Some sg
      | Some (Functor _) -> None (* TODO: Should be an error *)
    end

let expand_argument_ t root {FunctorArgument. id; expr; expansion} =
  match expansion with
  | None ->
      let id = Identifier.signature_of_module id in
      expand_module_type_expr t root id 0 expr
  | Some (Module.Signature sg) -> Some (Signature sg)
  | Some (Module.Functor _) ->
      (* CR trefis: This is for cases where the module argument is itself a functor.
         It *should* be handled, but latter. *)
      None

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
        expand_argument_ t root arg
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
      let {FunctorArgument. id; _} as arg = find_argument t root pos ex in
      let doc = DocOckAttrs.empty in
        id, doc, expand_argument_ t root arg

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

and expand_module_path' ({equal = eq} as t) root p =
  let open Path in
  match p with
  | Forward s -> t.expand_forward_ref ~root s
  | Dot(parent, name) ->
      let open Module in
      let id, _, ex = t.expand_module_path ~root parent in
      let md = find_module t root name ex in
      let sub = DocOckSubst.prefix ~equal:eq id in
      let md' = DocOckSubst.module_ sub md in
        md'.id, md'.doc, expand_module t root md'
  | Root _ | Apply _ | Resolved _ -> raise Not_found (* TODO: assert false? *)

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
    match unit.expansion with
    | Some ex -> Some ex
    | None ->
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
                          let md = {id; doc; type_; expansion = Some (Signature sg)} in
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


let create (type a) ?equal ?hash
      (lookup : string -> a DocOckComponentTbl.lookup_result)
      (fetch : root:a -> a -> a Unit.t) =
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
  let module RPathHash = struct
    type t = a * a Path.Resolved.any
    let equal (root1, p1) (root2, p2) =
      equal root1 root2 && Path.Resolved.equal ~equal p1 p2
    let hash (root, p) =
      Hashtbl.hash (hash root, Path.Resolved.hash ~hash)
  end in
  let module RPathTbl = Hashtbl.Make(RPathHash) in
  let module PathHash = struct
    type t = a * a Path.any
    let equal (root1, p1) (root2, p2) =
      equal root1 root2 && Path.equal ~equal p1 p2
    let hash (root, p) =
      Hashtbl.hash (hash root, Path.hash ~hash)
  end in
  let module PathTbl = Hashtbl.Make(PathHash) in
  let expand_module_resolved_path_tbl = RPathTbl.create 13 in
  let expand_module_path_tbl = PathTbl.create 13 in
  let expand_module_type_resolved_path_tbl = RPathTbl.create 13 in
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
  and expand_forward_ref ~root str =
    match lookup str with
    | DocOckComponentTbl.Found a -> expand_root ~root a
    | _ -> raise Not_found
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
      RPathTbl.find expand_module_resolved_path_tbl key
    with Not_found ->
      let res = expand_module_resolved_path' t root p in
      RPathTbl.add expand_module_resolved_path_tbl key res;
      res
  and expand_module_path ~root p =
    let key = (root, Path.any p) in
    try
      PathTbl.find expand_module_path_tbl key
    with Not_found ->
      let res = expand_module_path' t root p in
      PathTbl.add expand_module_path_tbl key res;
      res
  and expand_module_type_resolved_path ~root p =
    let key = (root, Path.Resolved.any p) in
    try
      RPathTbl.find expand_module_type_resolved_path_tbl key
    with Not_found ->
      let res = expand_module_type_resolved_path' t root p in
      RPathTbl.add expand_module_type_resolved_path_tbl key res;
      res
  and t =
    { equal; hash;
      expand_root; expand_forward_ref; expand_module_path;
      expand_module_identifier;
      expand_module_type_identifier;
      expand_signature_identifier;
      expand_module_resolved_path;
      expand_module_type_resolved_path; }
  in
    t

let rec force_expansion t root (ex : 'a partial_expansion option) =
  match ex with
  | None -> None
  | Some (Signature sg) -> Some (Module.Signature sg)
  | Some (Functor(arg, dest, offset, expr)) ->
      let arg = expand_argument t arg in
      let ex = expand_module_type_expr t root dest offset expr in
        match force_expansion t root ex with
        | None -> None
        | Some (Module.Signature sg) -> Some(Module.Functor([arg], sg))
        | Some (Module.Functor(args, sg)) ->
            Some(Module.Functor(arg :: args, sg))

and expand_argument t arg_opt =
  match arg_opt with
  | None -> arg_opt
  | Some ({FunctorArgument. id; expr; expansion} as arg) ->
      match expansion with
      | Some _ -> arg_opt
      | None ->
          let root = Identifier.module_root id in
          let expansion = force_expansion t root (expand_argument_ t root arg) in
            Some {FunctorArgument. id; expr; expansion}

let expand_module t md =
  let open Module in
    match md.expansion with
    | Some _ -> md
    | None ->
        let root = Identifier.module_root md.id in
        let expansion = force_expansion t root (expand_module t root md) in
          { md with expansion }

let expand_module_type t mty =
  let open ModuleType in
    match mty.expansion with
    | Some _ -> mty
    | None ->
        let root = Identifier.module_type_root mty.id in
        let expansion = force_expansion t root (expand_module_type t root mty) in
          { mty with expansion }

let expand_include t incl =
  let open Include in
    if incl.expansion.resolved then incl
    else begin
      let root = Identifier.signature_root incl.parent in
        match expand_include t root incl with
        | None -> incl
        | Some content ->
            let expansion = {content;resolved=true} in
              { incl with expansion }
    end

(*
let expand_unit t unit =
  let open Unit in
    match unit.expansion with
    | Some _ -> unit
    | None ->
        let root = Identifier.module_root unit.id in
        let expansion = expand_unit t root unit in
          { unit with expansion }
*)

class ['a] t ?equal ?hash lookup fetch = object (self)
  val t = create ?equal ?hash lookup fetch
  val unit = None

  inherit ['a] DocOckMaps.types as super
  method root x = x

  (* Define virtual methods. *)
  method identifier_module x = x
  method identifier_module_type x = x
  method identifier_type x = x
  method identifier_constructor x = x
  method identifier_field x = x
  method identifier_extension x = x
  method identifier_exception x = x
  method identifier_value x = x
  method identifier_class x = x
  method identifier_class_type x = x
  method identifier_method x = x
  method identifier_instance_variable x = x
  method identifier_label x = x
  method identifier_signature x = x
  method identifier x = x

  (* CR trefis: Is that ok? Probably. *)
  method path_module x = x
  method path_module_type x = x
  method path_type x = x
  method path_class_type x = x
  method fragment_type x = x
  method fragment_module x = x

  method module_ md =
    let md' = expand_module t md in
      super#module_ md'

  method module_type mty =
    let mty' = expand_module_type t mty in
      super#module_type mty'

  method include_ incl =
    let incl' = expand_include t incl in
    super#include_ incl'

  method module_type_functor_arg arg =
    let arg = expand_argument t arg in
      super#module_type_functor_arg arg


  (* CR trefis: TODO *)
  method reference_module x = x
  method reference_module_type x = x
  method reference_type x = x
  method reference_constructor x = x
  method reference_field x = x
  method reference_extension x = x
  method reference_exception x = x
  method reference_value x = x
  method reference_class x = x
  method reference_class_type x = x
  method reference_method x = x
  method reference_instance_variable x = x
  method reference_label x = x
  method reference_any x = x

  method expand unit =
    let this = {< unit = Some unit >} in
      this#unit unit
end

let build_expander ?equal ?hash lookup fetch =
  new t ?equal ?hash lookup fetch

let expand e u = e#expand u
