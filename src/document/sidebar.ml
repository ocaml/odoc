open Odoc_utils
open Types

let sidebar_toc_entry id content =
  let href =
    (id :> Odoc_model.Paths.Identifier.t)
    |> Url.from_identifier ~stop_before:false
    |> Result.get_ok
  in
  let target = Target.Internal (Resolved href) in
  inline @@ Inline.Link { target; content; tooltip = None }

module Toc : sig
  type t

  val of_page_hierarchy : Odoc_index.Page_hierarchy.t -> t

  val of_skeleton : Odoc_index.Skeleton.t -> t

  val to_block : prune:bool -> Url.Path.t -> t -> Block.t
end = struct
  type t = (Url.t * Inline.one) option Tree.t

  module Id = Odoc_model.Paths.Identifier

  let of_page_hierarchy (dir : Odoc_index.Page_hierarchy.t) =
    let fun_ index =
      let payload =
        match index with
        | None -> None
        | Some (index_id, title) ->
            let path =
              Url.from_identifier ~stop_before:false (index_id :> Id.t)
              |> Result.get_ok
            in
            let content = Comment.link_content title in
            Some (path, sidebar_toc_entry index_id content)
      in
      payload
    in
    Tree.map_t fun_ dir

  let rec is_prefix (url1 : Url.Path.t) (url2 : Url.Path.t) =
    if url1 = url2 then true
    else
      match url2 with
      | { parent = Some parent; _ } -> is_prefix url1 parent
      | { parent = None; _ } -> false

  let parent (url : Url.t) =
    match url with
    | { anchor = ""; page = { parent = Some parent; _ }; _ } -> parent
    | { page; _ } -> page

  let to_block ~prune (current_url : Url.Path.t) tree =
    let block_tree_of_t (current_url : Url.Path.t) tree =
      (* When transforming the tree, we use a filter_map to remove the nodes that
         are irrelevant for the current url. However, we always want to keep the
         root. So we apply the filter_map starting from the first children. *)
      let convert ((url : Url.t), b) =
        let link =
          if url.page = current_url && String.equal url.anchor "" then
            { b with Inline.attr = [ "current_unit" ] }
          else b
        in
        Types.block @@ Inline [ link ]
      in
      let fun_ name =
        match name with
        | Some ((url, _) as v)
          when (not prune) || is_prefix (parent url) current_url ->
            Some (convert v)
        | _ -> None
      in
      let root_entry =
        match tree.Tree.node with
        | Some v -> convert v
        | None -> block (Block.Inline [ inline (Text "root") ])
      in
      {
        Tree.node = root_entry;
        children = Tree.filter_map_f fun_ tree.children;
      }
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

  let of_skeleton ({ node = entry; children } : Odoc_index.Entry.t Tree.t) =
    let map_entry entry =
      let stop_before =
        match entry.Odoc_index.Entry.kind with
        | ModuleType { has_expansion } | Module { has_expansion } ->
            not has_expansion
        | _ -> false
      in
      let path = Url.from_identifier ~stop_before entry.id in
      let name = Odoc_model.Paths.Identifier.name entry.id in
      match path with
      | Ok path ->
          let content =
            let target = Target.Internal (Resolved path) in
            inline
              (Link { target; content = [ inline (Text name) ]; tooltip = None })
          in
          Some (path, content)
      | Error _ -> None
    in
    let fun_ entry =
      match entry.Odoc_index.Entry.kind with
      | Module _ | Class_type _ | Class _ | ModuleType _ ->
          Some (map_entry entry)
      | _ -> None
    in
    let entry = map_entry entry in
    let children = Tree.filter_map_f fun_ children in
    { Tree.node = entry; children }
end

type pages = { name : string; pages : Toc.t }
type library = { name : string; units : Toc.t list }

type t = { pages : pages list; libraries : library list }

let of_lang (v : Odoc_index.t) =
  let { Odoc_index.pages; libs; extra = _ } = v in
  let pages =
    let page_hierarchy { Odoc_index.p_name; p_hierarchy } =
      let hierarchy = Toc.of_page_hierarchy p_hierarchy in
      { name = p_name; pages = hierarchy }
    in
    Odoc_utils.List.map page_hierarchy pages
  in
  let libraries =
    let lib_hierarchies { Odoc_index.l_name; l_hierarchies } =
      let hierarchies = List.map Toc.of_skeleton l_hierarchies in
      { units = hierarchies; name = l_name }
    in
    Odoc_utils.List.map lib_hierarchies libs
  in
  { pages; libraries }

let to_block (sidebar : t) path =
  let { pages; libraries } = sidebar in
  let title t =
    block
      (Inline [ inline (Inline.Styled (`Bold, [ inline (Inline.Text t) ])) ])
  in
  let pages =
    Odoc_utils.List.concat_map
      ~f:(fun (p : pages) ->
        let pages = Toc.to_block ~prune:false path p.pages in
        let pages = [ block (Block.List (Block.Unordered, [ pages ])) ] in
        let pages = [ title @@ p.name ^ "'s Pages" ] @ pages in
        pages)
      pages
  in
  let units =
    let units =
      List.map
        (fun { units; name } ->
          let units =
            List.concat_map ~f:(Toc.to_block ~prune:true path) units
          in
          let units = [ block (Block.List (Block.Unordered, [ units ])) ] in
          let units = [ title @@ name ^ "'s Units" ] @ units in
          units)
        libraries
    in
    let units = block (Block.List (Block.Unordered, units)) in
    [ title "Libraries"; units ]
  in
  pages @ units
