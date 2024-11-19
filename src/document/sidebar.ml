open Odoc_utils
open Types
module Id = Odoc_model.Paths.Identifier

type entry = Url.t option * Inline.one

module Toc : sig
  type t = entry Tree.t

  val of_page_hierarchy : Odoc_index.Page_hierarchy.t -> t

  val to_block : prune:bool -> Url.Path.t -> t -> Block.t
end = struct
  type t = entry Tree.t

  let rec is_prefix (url1 : Url.Path.t) (url2 : Url.Path.t) =
    if url1 = url2 then true
    else
      match url2 with
      | { parent = Some parent; _ } -> is_prefix url1 parent
      | { parent = None; _ } -> false

  let parent (url : Url.t) =
    match url with
    | {
        anchor = "";
        page =
          {
            parent = Some { parent = Some parent; _ };
            name = "index";
            kind = `LeafPage;
          };
        _;
      }
    | { anchor = ""; page = { parent = Some parent; _ }; _ } ->
        parent
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

  let of_page_hierarchy ({ node = entry; children } : Odoc_index.Entry.t Tree.t)
      : t =
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
    let f index =
      match index.Odoc_index.Entry.kind with
      | Dir ->
          let path = Url.from_identifier ~stop_before:false index.id in
          Some (Some path, inline @@ Text (Id.name index.id))
      | Page _frontmatter ->
          let path =
            Url.from_identifier ~stop_before:false (index.id :> Id.t)
          in
          let title =
            match Odoc_model.Comment.find_zero_heading index.doc with
            | Some t -> t
            | None ->
                Odoc_model.Location_.[ at (span []) (`Word (Id.name index.id)) ]
          in
          let content = Comment.link_content title in
          let target = Target.Internal (Target.Resolved path) in
          let i = inline @@ Inline.Link { target; content; tooltip = None } in
          Some (Some path, i)
      | Module _ | Class_type _ | Class _ | ModuleType _ ->
          Some (map_entry index)
      | _ -> None
    in
    let entry = map_entry entry in
    let children = Forest.filter_map ~f children in
    { Tree.node = entry; children }
  (* Tree.filter_map ~f dir *)
end

type t = Toc.t list

let of_lang (v : Odoc_index.t) = List.map Toc.of_page_hierarchy v

let to_block (sidebar : t) path =
  List.map (Toc.to_block ~prune:true path) sidebar
