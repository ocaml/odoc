open Odoc_utils
open Paths.Identifier

module CPH = Hashtbl.ContainerPage
module LPH = Hashtbl.LeafPage

type page = Page.t
type leaf_page = LeafPage.t
type container_page = ContainerPage.t

module PageToc = struct
  type title = Comment.link_content
  type children_order = Frontmatter.child list Location_.with_location

  type payload = { title : title; children_order : children_order option }

  type dir_content = { leafs : payload LPH.t; dirs : in_progress CPH.t }
  and in_progress = container_page option * dir_content

  let empty_t dir_id = (dir_id, { leafs = LPH.create 10; dirs = CPH.create 10 })

  let get_parent id : container_page option =
    let id :> page = id in
    match id.iv with
    | `Page (Some parent, _) -> Some parent
    | `LeafPage (Some parent, _) -> Some parent
    | `Page (None, _) | `LeafPage (None, _) -> None

  let find_leaf ((_, dir_content) : in_progress) leaf_page =
    try Some (LPH.find dir_content.leafs leaf_page) with Not_found -> None

  let leafs (_, dir_content) =
    LPH.fold
      (fun id { title = payload; _ } acc ->
        if Astring.String.equal "index" (Paths.Identifier.name id) then acc
        else (id, payload) :: acc)
      dir_content.leafs []

  let dirs (_, dir_content) =
    CPH.fold (fun id payload acc -> (id, payload) :: acc) dir_content.dirs []

  let rec get_or_create (dir : in_progress) (id : container_page) : in_progress
      =
    let _, { dirs = parent_dirs; _ } =
      match get_parent id with
      | Some parent -> get_or_create dir parent
      | None -> dir
    in
    let current_item =
      try Some (CPH.find parent_dirs id) with Not_found -> None
    in
    match current_item with
    | Some item -> item
    | None ->
        let new_ = empty_t (Some id) in
        CPH.add parent_dirs id new_;
        new_

  let add (dir : in_progress) ((id : leaf_page), title, children_order) =
    let _, dir_content =
      match get_parent id with
      | Some parent -> get_or_create dir parent
      | None -> dir
    in
    LPH.replace dir_content.leafs id { title; children_order }

  let dir_index ((parent_id, _) as dir) =
    let index_id =
      Paths.Identifier.Mk.leaf_page (parent_id, Names.PageName.make_std "index")
    in
    match find_leaf dir index_id with
    | Some payload -> Some (payload, index_id, payload.title)
    | None -> None

  type index = Page.t * title
  type t = (Page.t * content) list * index option
  and content = Entry of title | Dir of t

  let rec t_of_in_progress (dir : in_progress) =
    let children_order, index =
      match dir_index dir with
      | Some ({ children_order; _ }, index_id, index_title) ->
          (children_order, Some (index_id, index_title))
      | None -> (None, None)
    in
    let ordered, unordered =
      let contents =
        let leafs =
          leafs dir
          |> List.map (fun (id, payload) -> ((id :> Page.t), Entry payload))
        in
        let dirs =
          dirs dir
          |> List.map (fun (id, payload) ->
                 ((id :> Page.t), Dir (t_of_in_progress payload)))
        in
        leafs @ dirs
      in
      match children_order with
      | None -> ([], contents)
      | Some children_order ->
          List.partition_map
            (fun (((id : Page.t), _) as entry) ->
              match
                List.find_index
                  (fun ch ->
                    match (ch, id.iv) with
                    | Frontmatter.Dir c, `Page (_, name) ->
                        String.equal (Names.PageName.to_string name) c
                    | Page c, `LeafPage (_, name) ->
                        String.equal (Names.PageName.to_string name) c
                    | _ -> false)
                  children_order.value
              with
              | Some i -> `Left (i, entry)
              | None -> `Right entry)
            contents
    in
    let () =
      match (children_order, unordered) with
      | Some x, (_ :: _ as l) ->
          let pp fmt (id, _) =
            match id.iv with
            | `LeafPage (_, name) ->
                Format.fprintf fmt "'%s'" (Names.PageName.to_string name)
            | `Page (_, name) ->
                Format.fprintf fmt "'%s/'" (Names.PageName.to_string name)
          in
          Error.raise_warning
            (Error.make "(children) doesn't include %a."
               (Format.pp_print_list pp) l (Location_.location x))
      | _ -> ()
    in
    let ordered =
      ordered
      |> List.sort (fun (i, _) (j, _) -> Int.compare i j)
      |> List.map snd
    in
    let unordered =
      List.sort
        (fun (x, _) (y, _) ->
          String.compare (Paths.Identifier.name x) (Paths.Identifier.name y))
        unordered
    in
    let contents = ordered @ unordered in
    (contents, index)

  let of_list l =
    let dir = empty_t None in
    List.iter (add dir) l;
    t_of_in_progress dir
end

type toc = PageToc.t

type library = { name : string; units : Paths.Identifier.RootModule.t list }

type page_hierarchy = { hierarchy_name : string; pages : toc }

type t = { pages : page_hierarchy list; libraries : library list }
