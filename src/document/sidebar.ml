open Odoc_utils
open Types
module Id = Odoc_model.Paths.Identifier

type entry = Url.t option * Inline.one

open Odoc_index

module Toc : sig
  type t = entry Tree.t

  val of_page_hierarchy : Page_hierarchy.t -> t

  val to_block : prune:bool -> Url.Path.t -> t -> Block.t
end = struct
  type t = entry Tree.t

  let rec is_prefix (url1 : Url.Path.t) (url2 : Url.Path.t) =
    match url1 with
    | { kind = `LeafPage; parent = None; name = "index" } -> true
    | { kind = `LeafPage; parent = Some p; name = "index" } -> is_prefix p url2
    | _ -> (
        if url1 = url2 then true
        else
          match url2 with
          | { parent = Some parent; _ } -> is_prefix url1 parent
          | { parent = None; _ } -> false)

  let to_block ~prune:_ (current_url : Url.Path.t) (tree : t) =
    let block_tree_of_t (current_url : Url.Path.t) (tree : t) =
      (* When transforming the tree, we use a filter_map to remove the nodes that
         are irrelevant for the current url. However, we always want to keep the
         root. So we apply the filter_map starting from the first children. *)
      let convert_entry ((url : Url.t option), b) =
        let link =
          match url with
          | Some url ->
              if url.page = current_url && Astring.String.equal url.anchor ""
              then { b with Inline.attr = [ "current_unit" ] }
              else b
          | None -> b
        in
        Types.block @@ Inline [ link ]
      in
      let rec convert n =
        let children =
          match n.Tree.node with
          | Some url, _ when not (is_prefix url.Url.Anchor.page current_url) ->
              []
          | _ -> List.map convert n.children
        in
        { Tree.node = convert_entry n.node; children }
      in
      convert tree
    in
    let rec block_of_block_tree { Tree.node = name; children = content } =
      let content =
        match content with
        | [] -> []
        | _ :: _ ->
            let content = List.map block_of_block_tree content in
            [ block (Block.List (Block.Unordered, content)) ]
      in
      name :: content
    in
    let block_tree = block_tree_of_t current_url tree in
    block_of_block_tree block_tree

  let of_page_hierarchy ({ node = entry; children } : Entry.t Tree.t) : t =
    let map_entry entry =
      match entry.Entry.kind with
      | Dir -> (None, inline @@ Text (Id.name entry.id))
      | _ ->
          let stop_before =
            match entry.Entry.kind with
            | ModuleType { has_expansion } | Module { has_expansion } ->
                not has_expansion
            | _ -> false
          in
          let path = Url.from_identifier ~stop_before (entry.id :> Id.t) in
          let content =
            match entry.kind with
            | Page _ ->
                let title =
                  let open Odoc_model in
                  match Comment.find_zero_heading entry.doc with
                  | Some t -> t
                  | None ->
                      Location_.[ at (span []) (`Word (Id.name entry.id)) ]
                in
                Comment.link_content title
            | _ ->
                let name = Odoc_model.Paths.Identifier.name entry.id in
                [ inline (Text name) ]
          in
          let target = Target.Internal (Target.Resolved path) in
          let i = inline @@ Inline.Link { target; content; tooltip = None } in
          (Some path, i)
    in
    let f x =
      match x.Entry.kind with
      | Dir | Page _ | Module _ | Class_type _ | Class _ | ModuleType _ ->
          Some (map_entry x)
      | _ -> None
    in
    let entry = map_entry entry in
    let children = Forest.filter_map ~f children in
    { Tree.node = entry; children }
end

type t = Toc.t list

let of_lang (v : Odoc_index.t) = List.map Toc.of_page_hierarchy v

let to_block (sidebar : t) path =
  List.map (Toc.to_block ~prune:true path) sidebar
