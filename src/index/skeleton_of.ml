open Odoc_utils
open Odoc_model

(* Selective opens *)
module Id = Odoc_model.Paths.Identifier
module PageName = Odoc_model.Names.PageName
module ModuleName = Odoc_model.Names.ModuleName

type t = Entry.t Tree.t

let rec t_of_in_progress (dir : In_progress.in_progress) : t =
  let entry_of_page page =
    let kind = Entry.Page page.Lang.Page.frontmatter in
    let doc = page.content in
    let id = page.name in
    Entry.entry ~kind ~doc ~id
  in
  let entry_of_impl id =
    let kind = Entry.Impl in
    let doc = [] in
    Entry.entry ~kind ~doc ~id
  in
  let children_order, index =
    match In_progress.index dir with
    | Some (_, page) ->
        let children_order = page.frontmatter.children_order in
        let entry = entry_of_page page in
        (children_order, entry)
    | None ->
        let entry =
          match In_progress.root_dir dir with
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
    | `Root (_, name) ->
        Format.fprintf fmt "'module-%s'" (ModuleName.to_string name)
    | _ -> Format.fprintf fmt "'unsupported'"
  in
  let pp_children fmt c =
    match c.Location_.value with
    | Frontmatter.Page s -> Format.fprintf fmt "'%s'" s
    | Dir s -> Format.fprintf fmt "'%s/'" s
    | Module s -> Format.fprintf fmt "'module-%s'" s
  in
  let ordered, unordered =
    let contents =
      let leafs =
        In_progress.leafs dir
        |> List.map (fun (_, page) ->
               let id :> Id.t = page.Lang.Page.name in
               let entry = entry_of_page page in
               (id, Tree.leaf entry))
      in
      let dirs =
        In_progress.dirs dir
        |> List.map (fun (id, payload) ->
               let id :> Id.t = id in
               (id, t_of_in_progress payload))
      in
      let modules =
        In_progress.modules dir
        |> List.map (fun (id, payload) -> ((id :> Id.t), payload))
      in
      let implementations =
        In_progress.implementations dir
        |> List.map (fun (id, _impl) ->
               ((id :> Id.t), Tree.leaf @@ entry_of_impl id))
      in
      leafs @ dirs @ modules @ implementations
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
          | (_, { Location_.value = Module c; _ }), `Root (_, name) ->
              String.equal (ModuleName.to_string name) c
          | _ -> false
        in
        let children_indexes, indexed_content, unindexed_content =
          List.fold_left
            (fun (children_indexes, indexed_content, unindexed_content)
                 ((id, _) as entry) ->
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
  | { Tree.children = [ v ]; node = { kind = Dir; _ } } -> remove_common_root v
  | _ -> v

let lang ~pages ~modules ~implementations =
  let dir = In_progress.empty_t None in
  List.iter (In_progress.add_page dir) pages;
  List.iter (In_progress.add_module dir) modules;
  List.iter (In_progress.add_implementation dir) implementations;
  t_of_in_progress dir |> remove_common_root
