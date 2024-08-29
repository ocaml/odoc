open Paths.Identifier

module CPH = Hashtbl.ContainerPage
module LPH = Hashtbl.LeafPage

type page = Page.t
type leaf_page = LeafPage.t
type container_page = ContainerPage.t

module PageToc = struct
  type title = Comment.link_content
  type children_order = Frontmatter.child list

  type payload = { title : title; children_order : children_order option }

  type dir_content = { leafs : payload LPH.t; dirs : t CPH.t }
  and t = container_page option * dir_content

  let empty_t dir_id = (dir_id, { leafs = LPH.create 10; dirs = CPH.create 10 })

  let get_parent id : container_page option =
    let id :> page = id in
    match id.iv with
    | `Page (Some parent, _) -> Some parent
    | `LeafPage (Some parent, _) -> Some parent
    | `Page (None, _) | `LeafPage (None, _) -> None

  let find_leaf ((_, dir_content) : t) leaf_page =
    try Some (LPH.find dir_content.leafs leaf_page) with Not_found -> None

  let find_dir (_, dir_content) container_page =
    try Some (CPH.find dir_content.dirs container_page) with Not_found -> None

  type content = Entry of title | Dir of t

  type c_or_l = Container of ContainerPage.t | Leaf of LeafPage.t

  let classify = function
    | { iv = `LeafPage _; _ } as id -> Leaf id
    | { iv = `Page _; _ } as id -> Container id

  let find dir id =
    let open Odoc_utils.OptionMonad in
    match classify id with
    | Leaf id -> find_leaf dir id >>= fun x -> Some (Entry x.title)
    | Container id -> find_dir dir id >>= fun x -> Some (Dir x)

  let leafs (_, dir_content) =
    LPH.fold
      (fun id { title = payload; _ } acc ->
        if String.equal "index" (Paths.Identifier.name id) then acc
        else (id, payload) :: acc)
      dir_content.leafs []

  let dir_payload ((parent_id, _) as dir) =
    let index_id =
      Paths.Identifier.Mk.leaf_page (parent_id, Names.PageName.make_std "index")
    in
    match find_leaf dir index_id with
    | Some payload -> Some (payload, index_id)
    | None -> None

  let dirs (_, dir_content) =
    CPH.fold (fun id payload acc -> (id, payload) :: acc) dir_content.dirs []

  let contents ((dir_id, _) as dir) =
    let children_order =
      match dir_payload dir with
      | Some ({ children_order; _ }, _) -> children_order
      | None -> None
    in
    let children_order =
      match children_order with
      | None ->
          let contents =
            let leafs =
              leafs dir
              |> List.map (fun (id, payload) -> ((id :> Page.t), Entry payload))
            in
            let dirs =
              dirs dir
              |> List.map (fun (id, payload) -> ((id :> Page.t), Dir payload))
            in
            leafs @ dirs
          in
          List.sort
            (fun (x, _) (y, _) ->
              String.compare (Paths.Identifier.name x) (Paths.Identifier.name y))
            contents
      | Some ch ->
          let open Odoc_utils.OptionMonad in
          List.filter_map
            (fun c ->
              let id =
                match c with
                | Frontmatter.Page name ->
                    Mk.leaf_page (dir_id, Names.PageName.make_std name)
                | Dir name -> Mk.page (dir_id, Names.PageName.make_std name)
              in
              find dir id >>= fun content -> Some (id, content))
            ch
    in
    children_order

  let dir_payload ((parent_id, _) as dir) =
    let index_id =
      Paths.Identifier.Mk.leaf_page (parent_id, Names.PageName.make_std "index")
    in
    match find_leaf dir index_id with
    | Some payload -> Some (payload.title, index_id)
    | None -> None

  let rec get_or_create (dir : t) (id : container_page) : t =
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

  let add (dir : t) ((id : leaf_page), title, children_order) =
    let _, dir_content =
      match get_parent id with
      | Some parent -> get_or_create dir parent
      | None -> dir
    in
    LPH.replace dir_content.leafs id { title; children_order }

  let of_list l =
    let dir = empty_t None in
    List.iter (add dir) l;
    dir
end

type toc = PageToc.t

type library = { name : string; units : Paths.Identifier.RootModule.t list }

type page_hierarchy = { hierarchy_name : string; pages : toc }

type t = { pages : page_hierarchy list; libraries : library list }
