open Odoc_utils
open Odoc_model

(* Selective opens *)
module Id = Odoc_model.Paths.Identifier
module PageName = Odoc_model.Names.PageName

module CPH = Id.Hashtbl.ContainerPage
module LPH = Id.Hashtbl.LeafPage
module RMH = Id.Hashtbl.RootModule

type page = Id.Page.t
type container_page = Id.ContainerPage.t

open Astring

type title = Comment.link_content

type payload = (* { title : title; frontmatter : Frontmatter.t } *) Lang.Page.t

type dir_content = {
  leafs : payload LPH.t;
  dirs : in_progress CPH.t;
  modules : Skeleton.t RMH.t;
}
and in_progress = container_page option * dir_content

let empty_t dir_id =
  ( dir_id,
    { leafs = LPH.create 10; dirs = CPH.create 10; modules = RMH.create 10 } )

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
      if String.equal "index" (Id.name id) then acc else (id, page) :: acc)
    dir_content.leafs []

let dirs (_, dir_content) =
  CPH.fold (fun id payload acc -> (id, payload) :: acc) dir_content.dirs []

let modules (_, dir_content) =
  RMH.fold (fun id payload acc -> (id, payload) :: acc) dir_content.modules []

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
  match page.Lang.Page.name with
  | { iv = #Id.LeafPage.t_pv; _ } as id ->
      let _, dir_content =
        match get_parent id with
        | Some parent -> get_or_create dir parent
        | None -> dir
      in
      LPH.replace dir_content.leafs id page
  | _ -> ()

let add_module (dir : in_progress) m =
  let _, dir_content =
    match m.Lang.Compilation_unit.id.iv with
    | `Root (Some parent, _) -> get_or_create dir parent
    | `Root (None, _) -> dir
  in
  let skel = Skeleton.from_unit m in
  RMH.replace dir_content.modules m.id skel

let dir_index ((parent_id, _) as dir) =
  let index_id = Id.Mk.leaf_page (parent_id, PageName.make_std "index") in
  match find_leaf dir index_id with
  | Some payload -> Some (index_id, payload)
  | None -> None

type t = Entry.t Tree.t

let rec t_of_in_progress (dir : in_progress) : t =
  let entry_of_page page =
    let kind = Entry.Page page.Lang.Page.frontmatter in
    let doc = page.content in
    let id = page.name in
    Entry.entry ~kind ~doc ~id
  in
  let children_order, index =
    match dir_index dir with
    | Some (_, page) ->
        let children_order = page.frontmatter.children_order in
        let entry = entry_of_page page in
        (children_order, entry)
    | None ->
        let entry =
          match fst dir with
          | Some id ->
              let kind = Entry.Dir in
              let doc = [] in
              Entry.entry ~kind ~doc ~id
          | None ->
              let id =
                (* root dir must have an index page *)
                Id.Mk.leaf_page (None, Names.PageName.make_std "index")
              in
              let kind = Entry.Dir in
              let doc = [] in
              Entry.entry ~kind ~doc ~id
        in
        (None, entry)
  in
  let pp_content fmt (id, _) =
    match id.Id.iv with
    | `LeafPage (_, name) -> Format.fprintf fmt "'%s'" (PageName.to_string name)
    | `Page (_, name) -> Format.fprintf fmt "'%s/'" (PageName.to_string name)
  in
  let pp_children fmt c =
    match c.Location_.value with
    | Frontmatter.Page s -> Format.fprintf fmt "'%s'" s
    | Dir s -> Format.fprintf fmt "'%s/'" s
  in
  let ordered, unordered =
    let contents =
      let leafs =
        leafs dir
        |> List.map (fun (_, page) ->
               let id :> Id.Page.t = page.Lang.Page.name in
               let entry = entry_of_page page in
               (id, Tree.leaf entry))
      in
      let dirs =
        dirs dir
        |> List.map (fun (id, payload) ->
               let id :> Id.Page.t = id in
               (id, t_of_in_progress payload))
      in
      leafs @ dirs
    in
    match children_order with
    | None -> ([], contents)
    | Some children_order ->
        let children_indexes =
          List.mapi (fun i x -> (i, x)) children_order.value
        in
        let equal id ch =
          match (ch, id.Id.iv) with
          | (_, { Location_.value = Frontmatter.Dir c; _ }), `Page (_, name) ->
              String.equal (PageName.to_string name) c
          | (_, { Location_.value = Page c; _ }), `LeafPage (_, name) ->
              String.equal (PageName.to_string name) c
          | _ -> false
        in
        let children_indexes, indexed_content, unindexed_content =
          List.fold_left
            (fun (children_indexes, indexed_content, unindexed_content)
                 (((id : Id.Page.t), _) as entry) ->
              let indexes_for_entry, children_indexes =
                List.partition (equal id) children_indexes
              in
              match indexes_for_entry with
              | [] ->
                  (children_indexes, indexed_content, entry :: unindexed_content)
              | (i, _) :: rest ->
                  List.iter
                    (fun (_, c) ->
                      Error.raise_warning
                        (Error.make "Duplicate %a in (children)." pp_children c
                           (Location_.location c)))
                    rest;
                  ( children_indexes,
                    (i, entry) :: indexed_content,
                    unindexed_content ))
            (children_indexes, [], []) contents
        in
        List.iter
          (fun (_, c) ->
            Error.raise_warning
              (Error.make "%a in (children) does not correspond to anything."
                 pp_children c (Location_.location c)))
          children_indexes;
        (indexed_content, unindexed_content)
  in
  let () =
    match (children_order, unordered) with
    | Some x, (_ :: _ as l) ->
        Error.raise_warning
          (Error.make "(children) doesn't include %a."
             (Format.pp_print_list pp_content)
             l (Location_.location x))
    | _ -> ()
  in
  let ordered =
    ordered
    |> List.sort (fun (i, _) (j, _) -> (compare : int -> int -> int) i j)
    |> List.map snd
  in
  let unordered =
    List.sort
      (fun (x, _) (y, _) ->
        String.compare (Paths.Identifier.name x) (Paths.Identifier.name y))
      unordered
  in
  let modules = modules dir |> List.map snd in
  let contents = ordered @ unordered |> List.map snd in
  { Tree.node = index (* , modules *); children = contents @ modules }

let rec remove_common_root (v : t) =
  match v with
  | { Tree.children = [ v ]; node = { kind = Dir; _ } } -> remove_common_root v
  | _ -> v

let of_list ~pages ~modules =
  let dir = empty_t None in
  List.iter (add_page dir) pages;
  List.iter (add_module dir) modules;
  t_of_in_progress dir |> remove_common_root
