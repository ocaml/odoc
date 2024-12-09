open Odoc_model

module Id = Odoc_model.Paths.Identifier
module PageName = Odoc_model.Names.PageName

module CPH = Id.Hashtbl.ContainerPage
module LPH = Id.Hashtbl.LeafPage
module RMH = Id.Hashtbl.RootModule
module SPH = Id.Hashtbl.SourcePage

type page = Id.Page.t
type container_page = Id.ContainerPage.t

type payload = Lang.Page.t

type dir_content = {
  leafs : payload LPH.t;
  dirs : in_progress CPH.t;
  modules : Skeleton.t RMH.t;
  implementations : Lang.Implementation.t SPH.t;
}
and in_progress = container_page option * dir_content

let empty_t dir_id =
  ( dir_id,
    {
      leafs = LPH.create 10;
      dirs = CPH.create 10;
      modules = RMH.create 10;
      implementations = SPH.create 10;
    } )

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
    (fun id page acc ->
      if Astring.String.equal "index" (Id.name id) then acc
      else (id, page) :: acc)
    dir_content.leafs []

let dirs (_, dir_content) =
  CPH.fold (fun id payload acc -> (id, payload) :: acc) dir_content.dirs []

let modules (_, dir_content) =
  RMH.fold (fun id payload acc -> (id, payload) :: acc) dir_content.modules []

let implementations (_, dir_content) =
  SPH.fold
    (fun id payload acc -> (id, payload) :: acc)
    dir_content.implementations []

let rec get_or_create (dir : in_progress) (id : container_page) : in_progress =
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

let add_page (dir : in_progress) page =
  let id =
    match page.Lang.Page.name with
    | { iv = #Id.ContainerPage.t_pv; _ } as id ->
        Id.Mk.leaf_page (Some id, PageName.make_std "index")
    | { iv = #Id.LeafPage.t_pv; _ } as id -> id
  in
  let _, dir_content =
    match get_parent id with
    | Some parent -> get_or_create dir parent
    | None -> dir
  in
  LPH.replace dir_content.leafs id page

let add_module (dir : in_progress) m =
  let _, dir_content =
    match m.Lang.Compilation_unit.id.iv with
    | `Root (Some parent, _) -> get_or_create dir parent
    | `Root (None, _) -> dir
  in
  let skel = Skeleton.from_unit m in
  RMH.replace dir_content.modules m.id skel

let add_implementation (dir : in_progress) (i : Lang.Implementation.t) =
  match i.id with
  | None -> ()
  | Some ({ iv = `SourcePage (parent, _); _ } as id) ->
      let _, dir_content = get_or_create dir parent in
      SPH.replace dir_content.implementations id i

let index ((parent_id, _) as dir) =
  let index_id = Id.Mk.leaf_page (parent_id, PageName.make_std "index") in
  match find_leaf dir index_id with
  | Some payload -> Some (index_id, payload)
  | None -> None

let root_dir (parent_id, _) = parent_id
