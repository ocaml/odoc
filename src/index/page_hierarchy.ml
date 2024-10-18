open Odoc_utils
open Odoc_model

(* Selective opens *)
module Id = Odoc_model.Paths.Identifier
module PageName = Odoc_model.Names.PageName

module CPH = Id.Hashtbl.ContainerPage
module LPH = Id.Hashtbl.LeafPage

type page = Id.Page.t
type leaf_page = Id.LeafPage.t
type container_page = Id.ContainerPage.t

open Astring

type title = Comment.link_content

type payload = {
  title : title;
  children_order : Frontmatter.children_order option;
}

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
      if String.equal "index" (Id.name id) then acc else (id, payload) :: acc)
    dir_content.leafs []

let dirs (_, dir_content) =
  CPH.fold (fun id payload acc -> (id, payload) :: acc) dir_content.dirs []

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

let add (dir : in_progress) ((id : leaf_page), title, children_order) =
  let _, dir_content =
    match get_parent id with
    | Some parent -> get_or_create dir parent
    | None -> dir
  in
  LPH.replace dir_content.leafs id { title; children_order }

let dir_index ((parent_id, _) as dir) =
  let index_id = Id.Mk.leaf_page (parent_id, PageName.make_std "index") in
  match find_leaf dir index_id with
  | Some payload -> Some (payload, index_id, payload.title)
  | None -> None

type index = Id.Page.t * title

type t = index option Odoc_utils.Tree.t

let rec t_of_in_progress (dir : in_progress) : t =
  let children_order, index =
    match dir_index dir with
    | Some ({ children_order; _ }, index_id, index_title) ->
        (children_order, Some (index_id, index_title))
    | None -> (None, None)
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
        |> List.map (fun (id, payload) ->
               let id :> Id.Page.t = id in
               (id, Tree.leaf (Some (id, payload))))
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
  let contents = ordered @ unordered |> List.map snd in
  { Tree.node = index; children = contents }

let rec remove_common_root (v : t) =
  match v with
  | { Tree.children = [ v ]; node = None } -> remove_common_root v
  | _ -> v

let of_list l =
  let dir = empty_t None in
  List.iter (add dir) l;
  t_of_in_progress dir |> remove_common_root
