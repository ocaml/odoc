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

let polarities typ =
  List.of_seq
  @@ Seq.filter
       (fun (word, _count, _) -> String.length word > 0)
       (Db.Type_polarity.of_typ ~any_is_poly:false typ)

let find_types ~shard typ =
  let polarities = polarities typ in
  Succ.inter_of_list
  @@ List.map
       (fun (name, count, polarity) ->
         let st_occ =
           match polarity with
           | Db.Type_polarity.Sign.Pos -> shard.Db.db_pos_types
           | Neg -> shard.Db.db_neg_types
         in
         Succ.union_of_list
         @@ Db.Occurences.fold
              (fun occurrences st acc ->
                if occurrences < count
                then acc
                else begin
                  let ts = Tree.find_star st name in
                  let ss = List.map Succ.of_automata ts in
                  List.rev_append ss acc
                end)
              st_occ
              [])
       polarities

let find_names ~shard names =
  let names = List.map String.lowercase_ascii names in
  let db_names = Db.(shard.db_names) in
  let candidates =
    List.map
      (fun name ->
        match Tree.find db_names name with
        | Some trie -> Succ.of_automata trie
        | None -> Succ.empty)
      names
  in
  Succ.inter_of_list candidates

let search ~shard { Query_parser.name; typ } =
  match name, typ with
  | _ :: _, `typ typ ->
    let results_name = find_names ~shard name in
    let results_typ = find_types ~shard typ in
    Succ.inter results_name results_typ
  | _ :: _, _ -> find_names ~shard name
  | [], `typ typ -> find_types ~shard typ
  | [], (`no_typ | `parse_error) -> Succ.empty

let search ~shards query =
  Succ.union_of_list (List.map (fun shard -> search ~shard query) shards)

type t =
  { query : string
  ; packages : string list
  ; limit : int
  }

let match_packages ~packages { Db.Entry.pkg; _ } =
  List.exists (String.equal pkg.name) packages

let match_packages ~packages results =
  match packages with
  | [] -> results
  | _ -> Seq.filter (match_packages ~packages) results

let rec seq_take n xs () =
  if n = 0
  then Seq.Nil
  else begin
    match xs () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (x, xs) -> Seq.Cons (x, seq_take (n - 1) xs)
  end

let search ~shards ?(dynamic_sort = true) params =
  let limit = params.limit in
  let query = Parser.of_string params.query in
  let results = search ~shards query in
  let results = Succ.to_seq results in
  let results = match_packages ~packages:params.packages results in
  if dynamic_sort
  then begin
    let query = Dynamic_cost.of_query query in
    List.of_seq @@ Top_results.of_seq ~query ~limit results
  end
  else List.of_seq @@ seq_take params.limit results

let pretty params = Parser.(to_string @@ of_string params.query)
