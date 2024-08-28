open Types

module Hierarchy : sig
  type 'a dir
  (** Directory in a filesystem-like abstraction, where files have a ['a]
      payload and directory can also have a ['a] payload. *)

  val make : ('a * string list) list -> 'a dir
  (** Create a directory from a list of payload and file path (given as a
      string list). Files named ["index"] give their payload to their
      containing directory. *)

  val remove_common_root : 'a dir -> 'a dir
  (** Returns the deepest subdir containing all files. *)

  val to_sidebar : ?fallback:string -> ('a -> Block.one) -> 'a dir -> Block.t
end = struct
  type 'a dir = 'a option * (string * 'a t) list
  and 'a t = Leaf of 'a | Dir of 'a dir

  let rec add_entry_to_dir (dir : 'a dir) payload path =
    match (path, dir) with
    | [], _ -> assert false
    | [ "index" ], (None, l) -> (Some payload, l)
    | [ name ], (p, l) -> (p, (name, Leaf payload) :: l)
    | name :: rest, (p, l) ->
        let rec add_to_dir (l : (string * 'a t) list) =
          match l with
          | [] -> [ (name, Dir (add_entry_to_dir (None, []) payload rest)) ]
          | (name2, Dir d) :: q when name = name2 ->
              (name2, Dir (add_entry_to_dir d payload rest)) :: q
          | d :: q -> d :: add_to_dir q
        in
        (p, add_to_dir l)

  let make l =
    let empty = (None, []) in
    let add_entry_to_dir acc (path, payload) =
      add_entry_to_dir acc path payload
    in
    List.fold_left add_entry_to_dir empty l

  let rec remove_common_root = function
    | None, [ (_, Dir d) ] -> remove_common_root d
    | x -> x

  let rec to_sidebar ?(fallback = "root") convert (name, content) =
    let name =
      match name with
      | Some v -> convert v
      | None -> block (Block.Inline [ inline (Text fallback) ])
    in
    let content =
      let content = List.map (t_to_sidebar convert) content in
      block (Block.List (Block.Unordered, content))
    in
    [ name; content ]

  and t_to_sidebar convert = function
    | _, Leaf payload -> [ convert payload ]
    | fallback, Dir d -> to_sidebar ~fallback convert d
end
type pages = { name : string; pages : (Url.Path.t * Inline.one) Hierarchy.dir }
type library = { name : string; units : (Url.Path.t * Inline.one) list }

type t = { pages : pages list; libraries : library list }

let of_lang (v : Odoc_model.Lang.Sidebar.t) =
  let sidebar_toc_entry id content =
    let href = id |> Url.Path.from_identifier |> Url.from_path in
    let target = Target.Internal (Resolved href) in
    inline @@ Inline.Link { target; content; tooltip = None }
  in
  let pages =
    let page_hierarchy { Odoc_model.Lang.Sidebar.page_name; pages } =
      if pages = [] then None
      else
        let prepare_for_hierarchy { Odoc_model.Lang.Sidebar.title; id } =
          let path = Url.Path.from_identifier id in
          let payload =
            let content = Comment.link_content title in
            (path, sidebar_toc_entry id content)
          in
          (payload, path |> Url.Path.to_list |> List.map snd)
        in
        let pages = List.map prepare_for_hierarchy pages in
        let hierarchy = Hierarchy.make pages |> Hierarchy.remove_common_root in
        Some { name = page_name; pages = hierarchy }
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
        (fun { Odoc_model.Lang.Sidebar.units; name } ->
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
        let pages = Hierarchy.to_sidebar render_entry p.pages in
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
