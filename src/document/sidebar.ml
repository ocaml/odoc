open Odoc_utils
open Types
module Id = Odoc_model.Paths.Identifier

let sidebar_toc_entry id content =
  let target =
    match
      (id :> Odoc_model.Paths.Identifier.t)
      |> Url.from_identifier ~stop_before:false
    with
    | Ok href -> Target.Resolved href
    | Error _ -> Target.Unresolved
    (* This error case should never happen since [stop_before] is false *)
  in
  let target = Target.Internal target in
  inline @@ Inline.Link { target; content; tooltip = None }

module Toc : sig
  type t

  val of_page_hierarchy : Odoc_index.Page_hierarchy.t -> t

  val to_sidebar :
    ?fallback:string -> (Url.t * Inline.one -> Block.one) -> t -> Block.t
end = struct
  type t = (Url.t * Inline.one) option Tree.t

  let of_page_hierarchy (dir : Odoc_index.Page_hierarchy.t) =
    let f index =
      let payload =
        match index with
        | None -> None
        | Some (index_id, title) ->
            let path =
              match
                Url.from_identifier ~stop_before:false (index_id :> Id.t)
              with
              | Ok r -> r
              | Error _ -> assert false
              (* This error case should never happen since [stop_before] is false, and even less since it's a page id *)
            in
            let content = Comment.link_content title in
            Some (path, sidebar_toc_entry index_id content)
      in
      payload
    in
    Tree.map ~f dir

  let rec to_sidebar ?(fallback = "root") convert
      { Tree.node = name; children = content } =
    let name =
      match name with
      | Some v -> convert v
      | None -> block (Block.Inline [ inline (Text fallback) ])
    in
    let content =
      match content with
      | [] -> []
      | _ :: _ ->
          let content = List.map (to_sidebar convert) content in
          [ block (Block.List (Block.Unordered, content)) ]
    in
    name :: content
end
type pages = { name : string; pages : Toc.t }
type library = { name : string; units : (Url.t * Inline.one) list }

type t = { pages : pages list; libraries : library list }

let of_lang (v : Odoc_index.sidebar) =
  let pages =
    let page_hierarchy { Odoc_index.p_name; p_hierarchy } =
      let hierarchy = Toc.of_page_hierarchy p_hierarchy in
      Some { name = p_name; pages = hierarchy }
    in
    Odoc_utils.List.filter_map page_hierarchy v.pages
  in
  let units =
    let item id =
      let content = [ inline @@ Text (Odoc_model.Paths.Identifier.name id) ] in
      let path = Url.from_identifier ~stop_before:false (id :> Id.t) in
      match path with
      | Ok path -> Some (path, sidebar_toc_entry id content)
      | Error _ -> None
    in
    let units =
      List.map
        (fun { Odoc_index.units; name } ->
          let units = List.filter_map item units in
          { name; units })
        v.libs
    in
    units
  in
  { pages; libraries = units }

let to_block (sidebar : t) url =
  let { pages; libraries } = sidebar in
  let title t =
    block
      (Inline [ inline (Inline.Styled (`Bold, [ inline (Inline.Text t) ])) ])
  in
  let render_entry (entry_path, b) =
    let link =
      if entry_path = Url.from_path url then
        { b with Inline.attr = [ "current_unit" ] }
      else b
    in
    Types.block @@ Inline [ link ]
  in
  let pages =
    Odoc_utils.List.concat_map
      ~f:(fun (p : pages) ->
        let pages = Toc.to_sidebar render_entry p.pages in
        let pages = [ block (Block.List (Block.Unordered, [ pages ])) ] in
        let pages = [ title @@ p.name ^ "'s Pages" ] @ pages in
        pages)
      pages
  in
  let units =
    let units =
      List.map
        (fun { units; name } ->
          [
            title name;
            block (List (Block.Unordered, [ List.map render_entry units ]));
          ])
        libraries
    in
    let units = block (Block.List (Block.Unordered, units)) in
    [ title "Libraries"; units ]
  in
  pages @ units
