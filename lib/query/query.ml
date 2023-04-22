module Parser = Query_parser
module Succ = Succ
module Sort = Sort
module Storage = Db.Storage
open Db.Types

let inter_list = function
  | [] -> Succ.all
  | x :: xs -> List.fold_left Succ.inter x xs

let collapse_trie t =
  let open Db.Types.T in
  match t with
  | Leaf (_, outcome) -> outcome
  | Node { summary = Some s; _ } -> s
  | _ -> Occ.empty

let collapse_trie t =
  let r = collapse_trie t in
  let r = Occ.map Succ.of_set r in
  r

let collapse_triechar t _acc =
  let open Tchar in
  match t with
  | Leaf (_, outcome) -> outcome
  | Node { summary = Some s; _ } -> s
  | _ -> Elt_set.empty

let collapse_triechar t = Succ.of_set (collapse_triechar t Elt_set.empty)

let collapse_count ~count (t : Succ.t Occ.t) =
  Occ.fold
    (fun k x acc -> if k < count then acc else Succ.union x acc)
    t Succ.empty

let collapse_trie_with_poly name t =
  match name with
  | [ "POLY"; _ ] ->
      let open T in
      begin
        match t with
        | Leaf ([], s) | Node { leaf = Some s; _ } -> Occ.map Succ.of_set s
        | _ -> Occ.empty
      end
  | _ -> collapse_trie t

let sort x = x

let find_inter ~shards names =
  List.fold_left
    (fun acc shard ->
      let db = shard.Storage.db in
      let r =
        sort @@ inter_list
        @@ List.map
             (fun (name, count) ->
               let name' = List.concat_map Db.list_of_string name in
               collapse_count ~count
               @@ collapse_trie_with_poly name
               @@ T.find name' db)
             (regroup names)
      in
      Succ.union acc r)
    Succ.empty shards

let find_names ~shards names =
  let names =
    List.map
      (fun n -> List.rev (Db.list_of_string (String.lowercase_ascii n)))
      names
  in
  List.fold_left
    (fun acc shard ->
      let db_names = shard.Storage.db_names in
      let candidates =
        List.map
          (fun name ->
            let t = Tchar.find name db_names in
            collapse_triechar t)
          names
      in
      let candidates = inter_list candidates in
      Succ.union acc candidates)
    Succ.empty shards

type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

let search ~shards query_name query_typ =
  let results_name = find_names ~shards query_name in
  let results =
    match query_typ with
    | None -> results_name
    | Some query_typ ->
        let results_typ = find_inter ~shards query_typ in
        Succ.inter results_name results_typ
  in
  results

let match_packages ~packages { Db.Elt.pkg = package, _version; _ } =
  List.exists (String.equal package) packages

let match_packages ~packages results =
  match packages with
  | [] -> results
  | _ -> Seq.filter (match_packages ~packages) results

let api ~shards params =
  let query_name, query_typ, query_typ_arrow, pretty =
    Parser.of_string params.query
  in
  let results = search ~shards query_name query_typ in
  let results = Succ.to_seq results in
  let results = match_packages ~packages:params.packages results in
  let results = List.of_seq @@ Seq.take params.limit results in
  let results = Sort.list query_name query_typ_arrow results in
  pretty, results
