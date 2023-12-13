module Parser = Query_parser
module Dynamic_cost = Dynamic_cost
module Storage = Db.Storage
module Tree = Db.Suffix_tree.With_elts
module Tree_occ = Db.Suffix_tree.With_occ
open Db.Types
module Occ = Db.Occ

module Private = struct
  module Array_succ = Array_succ
  module Succ = Succ
end

let collapse_occ ~count occs =
  Occ.fold
    (fun k x acc -> if k < count then acc else Succ.union (Succ.of_array x) acc)
    occs Succ.empty

let collapse_trie_occ ~count t =
  Succ.(
    Tree_occ.sets_tree ~union ~terminal:(collapse_occ ~count) ~union_of_array t)

let collapse_trie t =
  Succ.(Tree.sets_tree ~union ~terminal:of_array ~union_of_array t)

let find_types ~shards names =
  List.fold_left
    (fun acc shard ->
      let db = shard.db_types in
      let r =
        Succ.inter_of_list
        @@ List.map
             (fun (name, count) ->
               let name' = String.concat "" name in
               match Tree_occ.find db name' with
               | Some trie -> collapse_trie_occ ~count trie
               | None -> Succ.empty)
             (Db.Typepath.regroup names)
      in
      Succ.union acc r)
    Succ.empty shards

let find_names ~(shards : Db.t list) names =
  let names = List.map String.lowercase_ascii names in
  List.fold_left
    (fun acc shard ->
      let db_names = shard.db_names in
      let candidates =
        List.map
          (fun name ->
            match Tree.find db_names name with
            | Some trie -> collapse_trie trie
            | None -> Succ.empty)
          names
      in
      let candidates = Succ.inter_of_list candidates in
      Succ.union acc candidates)
    Succ.empty shards

type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

let search ~(shards : Db.t list) query_name query_typ =
  match query_name, query_typ with
  | [], None -> Succ.empty
  | _ :: _, None -> find_names ~shards query_name
  | [], Some query_typ -> find_types ~shards query_typ
  | _ :: _, Some query_typ ->
      let results_name = find_names ~shards query_name in

      let results_typ = find_types ~shards query_typ in
      Succ.inter results_name results_typ

let match_packages ~packages { Db.Elt.pkg; _ } =
  match pkg with
  | Some { name; version = _ } -> List.exists (String.equal name) packages
  | None -> false

let match_packages ~packages results =
  match packages with
  | [] -> results
  | _ -> Seq.filter (match_packages ~packages) results

let api ~(shards : Db.t list) ?(dynamic_sort = true) params =
  let query_name, query_typ, query_typ_arrow, pretty =
    Parser.of_string params.query
  in
  let results = search ~shards query_name query_typ in
  let results = Succ.to_seq ~compare:Db.Elt.compare results in
  let results = match_packages ~packages:params.packages results in
  let results = List.of_seq @@ Seq.take params.limit results in
  let results =
    if dynamic_sort
    then
      List.map
        (Dynamic_cost.elt ~query_name ~query_type:query_typ_arrow)
        results
    else results
  in
  let results = List.sort Db.Elt.compare results in

  pretty, results
