open Odoc_utils
open Types
module Id = Odoc_model.Paths.Identifier

type entry = Url.t option * Inline.one

module Toc : sig
  type t = entry Tree.t

  val of_page_hierarchy : Odoc_index.Page_hierarchy.t -> t

  val of_skeleton : Odoc_index.Skeleton.t -> t

  val to_block : prune:bool -> Url.Path.t -> t -> Block.t
end = struct
  type t = entry Tree.t

  let of_page_hierarchy (dir : Odoc_index.Page_hierarchy.t) : t =
    let f index =
      match index with
      | Odoc_index.Page_hierarchy.Missing_index None ->
          (None, inline @@ Text "Root")
      | Odoc_index.Page_hierarchy.Missing_index (Some id) ->
          let path = Url.from_identifier ~stop_before:false (id :> Id.t) in
          (Some path, inline @@ Text (Id.name id))
      | Page (id, title) ->
          let path = Url.from_identifier ~stop_before:false (id :> Id.t) in
          let content = Comment.link_content title in
          let target = Target.Internal (Target.Resolved path) in
          let i = inline @@ Inline.Link { target; content; tooltip = None } in
          (Some path, i)
    in
    Tree.map ~f dir

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

  let to_block ~prune (current_url : Url.Path.t) (tree : t) =
    let block_tree_of_t (current_url : Url.Path.t) (tree : t) =
      (* When transforming the tree, we use a filter_map to remove the nodes that
         are irrelevant for the current url. However, we always want to keep the
         root. So we apply the filter_map starting from the first children. *)
      let convert ((url : Url.t option), b) =
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
      let f name =
        match name with
        | Some url, _ when prune && not (is_prefix (parent url) current_url) ->
            None
        | v -> Some (convert v)
      in
      let root_entry = convert tree.Tree.node in
      { Tree.node = root_entry; children = Forest.filter_map ~f tree.children }
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

  let of_skeleton ({ node = entry; children } : Odoc_index.Entry.t Tree.t) : t =
    let map_entry entry =
      let stop_before =
        match entry.Odoc_index.Entry.kind with
        | ModuleType { has_expansion } | Module { has_expansion } ->
            not has_expansion
        | _ -> false
      in
      let name = Odoc_model.Paths.Identifier.name entry.id in
      let path = Url.from_identifier ~stop_before entry.id in
      let content =
        let target = Target.Internal (Resolved path) in
        inline
          (Link { target; content = [ inline (Text name) ]; tooltip = None })
      in
      (Some path, content)
    in
    let f entry =
      match entry.Odoc_index.Entry.kind with
      | Module _ | Class_type _ | Class _ | ModuleType _ ->
          Some (map_entry entry)
      | _ -> None
    in
    let entry = map_entry entry in
    let children = Forest.filter_map ~f children in
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
  let title t = block (Inline [ inline (Inline.Styled (`Bold, t)) ]) in
  let pages =
    let pages =
      Odoc_utils.List.concat_map
        ~f:(fun (p : pages) ->
          let () = ignore p.name in
          let pages = Toc.to_block ~prune:false path p.pages in
          [
            block ~attr:[ "odoc-pages" ]
              (Block.List (Block.Unordered, [ pages ]));
          ])
        pages
    in
    [ title @@ [ inline (Inline.Text "Documentation") ] ] @ pages
  in
  let units =
    let units =
      List.map
        (fun { units; name } ->
          let units =
            List.concat_map ~f:(Toc.to_block ~prune:true path) units
          in
          let units = [ block (Block.List (Block.Unordered, [ units ])) ] in
          [
            title
            @@ [
                 inline (Inline.Text "Library ");
                 inline (Inline.Source [ Elt [ inline @@ Text name ] ]);
               ];
          ]
          @ units)
        libraries
    in
    let units =
      block ~attr:[ "odoc-modules" ] (Block.List (Block.Unordered, units))
    in
    [ units ]
  in
  units @ pages
