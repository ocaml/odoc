open Odoc_utils
open Types

let sidebar_toc_entry id content =
  let href = id |> Url.Path.from_identifier |> Url.from_path in
  let target = Target.Internal (Resolved href) in
  inline @@ Inline.Link { target; content; tooltip = None }

module Toc : sig
  type t

  val of_lang : Odoc_model.Sidebar.toc -> t

  val remove_common_root : t -> t
  (** Returns the deepest subdir containing all files. *)

  val to_sidebar :
    ?fallback:string -> (Url.Path.t * Inline.one -> Block.one) -> t -> Block.t
end = struct
  type t = Item of (Url.Path.t * Inline.one) option * t list

  open Odoc_model.Sidebar
  open Odoc_model.Paths.Identifier

  let of_lang (dir : toc) =
    let rec of_lang ~parent_id (dir : toc) =
      let title, parent_id =
        match PageToc.dir_payload dir with
        | Some (title, index_id) -> (Some title, Some (index_id :> Page.t))
        | None -> (None, (parent_id :> Page.t option))
      in
      let children_order = PageToc.contents dir in
      let entries =
        List.filter_map
          (fun id ->
            match id with
            | id, PageToc.Entry title ->
                (* TODO warn on non empty children order if not index page somewhere *)
                let payload =
                  let path = Url.Path.from_identifier id in
                  let content = Comment.link_content title in
                  Some (path, sidebar_toc_entry id content)
                in
                Some (Item (payload, []))
            | id, PageToc.Dir dir -> Some (of_lang ~parent_id:(Some id) dir))
          children_order
      in
      let payload =
        match (title, parent_id) with
        | None, _ | _, None -> None
        | Some title, Some parent_id ->
            let path = Url.Path.from_identifier parent_id in
            let content = Comment.link_content title in
            Some (path, sidebar_toc_entry parent_id content)
      in
      Item (payload, entries)
    in

    of_lang ~parent_id:None dir

  let rec remove_common_root = function
    | Item (_, [ d ]) -> remove_common_root d
    | x -> x

  let rec to_sidebar ?(fallback = "root") convert (Item (name, content)) =
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
type library = { name : string; units : (Url.Path.t * Inline.one) list }

type t = { pages : pages list; libraries : library list }

let of_lang (v : Odoc_model.Sidebar.t) =
  let pages =
    let page_hierarchy { Odoc_model.Sidebar.hierarchy_name; pages } =
      let hierarchy = Toc.of_lang pages |> Toc.remove_common_root in
      Some { name = hierarchy_name; pages = hierarchy }
    in
    Odoc_utils.List.filter_map page_hierarchy v.pages
  in
  let units =
    let item id =
      let content = [ inline @@ Text (Odoc_model.Paths.Identifier.name id) ] in
      (Url.Path.from_identifier id, sidebar_toc_entry id content)
    in
    let units =
      List.map
        (fun { Odoc_model.Sidebar.units; name } ->
          let units = List.map item units in
          { name; units })
        v.libraries
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
      if entry_path = url then { b with Inline.attr = [ "current_unit" ] }
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
