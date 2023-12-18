module Parser = Query_parser
module Dynamic_cost = Dynamic_cost
module Storage = Db.Storage
module Tree = Db.Suffix_tree.With_elts
module Tree_occ = Db.Suffix_tree.With_occ
module Occ = Db.Occ

module Private = struct
  module Array_succ = Array_succ
  module Succ = Succ
  module Type_parser = Type_parser
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

let polarities typ =
  List.filter
    (fun (word, _count) -> String.length word > 0)
    (Db.Type_polarity.of_typ ~ignore_any:true ~all_names:false typ)

let find_types ~shards typ =
  let polarities = polarities typ in
  List.fold_left
    (fun acc shard ->
      let db = Db.(shard.db_types) in
      let r =
        Succ.inter_of_list
        @@ List.map
             (fun (name, count) ->
               match Tree_occ.find db name with
               | Some trie -> collapse_trie_occ ~count trie
               | None -> Succ.empty)
             polarities
      in
      Succ.union acc r)
    Succ.empty shards

let find_names ~(shards : Db.t list) names =
  let names = List.map String.lowercase_ascii names in
  List.fold_left
    (fun acc shard ->
      let db_names = Db.(shard.db_names) in
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
  | [], Error _ -> Succ.empty
  | _ :: _, Error _ -> find_names ~shards query_name
  | [], Ok query_typ -> find_types ~shards query_typ
  | _ :: _, Ok query_typ ->
      let results_name = find_names ~shards query_name in
      let results_typ = find_types ~shards query_typ in
      Succ.inter results_name results_typ

let match_packages ~packages { Db.Entry.pkg; _ } =
  match pkg with
  | Some { name; version = _ } -> List.exists (String.equal name) packages
  | None -> false

let match_packages ~packages results =
  match packages with
  | [] -> results
  | _ -> Seq.filter (match_packages ~packages) results

let search ~(shards : Db.t list) ?(dynamic_sort = true) params =
  let words, typ = Parser.of_string params.query in
  let results = search ~shards words typ in
  let results = Succ.to_seq ~compare:Db.Entry.compare results in
  let results = match_packages ~packages:params.packages results in
  let results = List.of_seq @@ Seq.take params.limit results in
  let results =
    if dynamic_sort
    then
      List.map
        (Dynamic_cost.update_entry ~query_name:words ~query_type:typ)
        results
    else results
  in
  let results = List.sort Db.Entry.compare results in
  results

let pretty params =
  let words, typ = Parser.of_string params.query in
  let words = String.concat " " words in
  match typ with
  | Ok typ -> words ^ " : " ^ Db.Typexpr.show typ
  | Error `parse -> words ^ " : <parsing error>"
  | Error `any -> words ^ " : _"
  | Error `empty -> words
