open Common
module Parser = Query_parser
module Succ = Succ
module Sort = Sort
module Storage = Db.Storage
module Trie = Db.Trie
open Db.Types
module Occ = Int.Map

let inter_list xs = List.fold_left Succ.inter Succ.all xs

let collapse_count ~count occs =
  Occ.fold
    (fun k x acc -> if k < count then acc else Succ.union (Succ.of_array x) acc)
    occs Succ.empty

let collapse_trie ~count t =
  match Trie.fold_map Succ.union (collapse_count ~count) t with
  | None -> Succ.empty
  | Some occ -> occ

let collapse_triechar t =
  match Trie.fold_map Succ.union Succ.of_array t with
  | None -> Succ.empty
  | Some s -> s

let collapse_trie_with_poly ~count name t =
  match name with
  | [ "POLY"; _ ] -> begin
      match t with
      | Trie.Leaf ([], s) | Node { leaf = Some s; _ } -> collapse_count ~count s
      | _ -> Succ.empty
    end
  | _ -> collapse_trie ~count t

let find_succ trie name collapse =
  match Trie.find name trie with
  | Some trie -> collapse trie
  | None -> Succ.empty

let find_inter ~shards names =
  List.fold_left
    (fun acc shard ->
      let db = shard.db_types in
      let r =
        inter_list
        @@ List.map
             (fun (name, count) ->
               let name' = List.concat_map Db.list_of_string name in
               find_succ db name' (collapse_trie_with_poly ~count name))
             (regroup names)
      in
      Succ.union acc r)
    Succ.empty shards

let find_names ~(shards : Db.Elt.t array Db.t list) names =
  let names =
    List.map
      (fun n -> List.rev (Db.list_of_string (String.lowercase_ascii n)))
      names
  in
  List.fold_left
    (fun acc shard ->
      let db_names = shard.db_names in
      let candidates =
        List.map (fun name -> find_succ db_names name collapse_triechar) names
      in
      let candidates = inter_list candidates in
      Succ.union acc candidates)
    Succ.empty shards

type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

let search ~(shards : Db.Elt.t array Db.t list) query_name query_typ =
  let results_name = find_names ~shards query_name in
  let results =
    match query_typ with
    | None -> results_name
    | Some query_typ ->
        let results_typ = find_inter ~shards query_typ in
        Succ.inter results_name results_typ
  in
  results

let match_packages ~packages { Db.Elt.pkg; _ } =
  match pkg with
  | Some { name; version = _ } -> List.exists (String.equal name) packages
  | None -> false

let match_packages ~packages results =
  match packages with
  | [] -> results
  | _ -> Seq.filter (match_packages ~packages) results

let api ~(shards : Db.Elt.t array Db.t list) params =
  let query_name, query_typ, query_typ_arrow, pretty =
    Parser.of_string params.query
  in
  let results = search ~shards query_name query_typ in
  let results = Succ.to_seq results in
  let results = match_packages ~packages:params.packages results in
  let results = List.of_seq @@ Seq.take params.limit results in
  let results = Sort.list query_name query_typ_arrow results in
  pretty, results
