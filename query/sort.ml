module Elt = Db.Types.Elt

let is_substring ~sub s =
  let re = Re.(compile (seq [ rep any; str sub ])) in
  Re.execp re s

let score_name query_name name =
  if String.starts_with ~prefix:query_name name
     || String.ends_with ~suffix:query_name name
  then 1
  else if is_substring ~sub:("(" ^ query_name) name
          || is_substring ~sub:(query_name ^ ")") name
  then 1
  else if is_substring ~sub:("." ^ query_name) name
          || is_substring ~sub:(query_name ^ ".") name
  then 2
  else if is_substring ~sub:("_" ^ query_name) name
          || is_substring ~sub:(query_name ^ "_") name
  then 3
  else 4

let score_name query_name name =
  match score_name query_name name with
  | 4 ->
      let query_name_lower = String.lowercase_ascii query_name in
      let name_lower = String.lowercase_ascii name in
      3
      + (if query_name = query_name_lower then 0 else 100)
      + score_name query_name_lower name_lower
  | c -> c

let score_name query_name name =
  List.fold_left
    (fun acc query_name -> acc + score_name query_name name)
    0 query_name

let by_name query_name results =
  let results =
    List.map
      (fun a ->
        let cost = a.Elt.cost + (2 * score_name query_name a.Elt.name) in
        { a with cost })
      results
  in
  List.sort Elt.compare results
