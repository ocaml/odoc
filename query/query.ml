module Parser = Query_parser
module Dynamic_cost = Dynamic_cost
module Storage = Db.Storage
module Tree = Db.String_automata

module Private = struct
  module Array_succ = Array_succ
  module Succ = Succ

  module Type_parser = struct
    let of_string str =
      let lexbuf = Lexing.from_string str in
      Ok (Type_parser.main Type_lexer.token lexbuf)
  end
end

let collapse_trie t =
  Succ.(Tree.sets_tree ~union ~terminal:of_array_opt ~union_of_array t)

let polarities typ =
  List.of_seq
  @@ Seq.filter
       (fun (word, _count, _) -> String.length word > 0)
       (Db.Type_polarity.of_typ ~any_is_poly:false ~all_names:false typ)

let find_types ~shards typ =
  let polarities = polarities typ in
  List.fold_left
    (fun acc shard ->
      let r =
        Succ.inter_of_list
        @@ List.map
             (fun (name, count, polarity) ->
               let st_occ =
                 match polarity with
                 | Db.Type_polarity.Sign.Pos -> shard.Db.db_pos_types
                 | Neg -> shard.Db.db_neg_types
               in
               Db.Occurences.fold
                 (fun occurrences st acc ->
                   if occurrences < count
                   then acc
                   else begin
                     match Tree.find st name with
                     | Some trie -> Succ.union acc (collapse_trie trie)
                     | None -> acc
                   end)
                 st_occ
                 Succ.empty)
             polarities
      in
      Succ.union acc r)
    Succ.empty
    shards

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
    Succ.empty
    shards

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
  List.exists (String.equal pkg.name) packages

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
    then List.map (Dynamic_cost.update_entry ~query_name:words ~query_type:typ) results
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
