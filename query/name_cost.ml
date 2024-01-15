let rec prefix_at ~case ~sub i s j =
  if i >= String.length sub
  then Some case
  else if sub.[i] = s.[j]
  then prefix_at ~case ~sub (i + 1) s (j + 1)
  else if sub.[i] = Char.lowercase_ascii s.[j]
  then prefix_at ~case:(case + 5) ~sub (i + 1) s (j + 1)
  else if Char.lowercase_ascii sub.[i] = s.[j]
  then prefix_at ~case:(case + 10) ~sub (i + 1) s (j + 1)
  else None

let prefix_at ~sub s j = prefix_at ~case:0 ~sub 0 s j

let find_all ~sub s =
  let rec go j acc =
    if j + String.length sub > String.length s
    then acc
    else begin
      let acc =
        match prefix_at ~sub s j with
        | None -> acc
        | Some cost -> (j, cost) :: acc
      in
      go (j + 1) acc
    end
  in
  go 0 []

let is_substring ~sub s = find_all ~sub s <> []

let word_boundary s i =
  if i < 0
  then 0
  else if i >= String.length s || List.mem s.[i] [ '.'; '('; ')' ]
  then 1
  else if s.[i] = '_'
  then 3
  else 10

let best_match ?(after = 0) ~sub str =
  List.fold_left
    (fun acc (i, case_cost) ->
      let left = word_boundary str (i - 1) in
      let right = word_boundary str (i + String.length sub) in
      let is_after = if i >= after then 0 else 10 in
      let cost = case_cost + left + right + is_after in
      match acc with
      | Some (_, cost') when cost' < cost -> acc
      | _ -> Some (i, cost))
    None
    (find_all ~sub str)

let best_matches words str =
  let _, found, not_found =
    List.fold_left
      (fun (i, found, not_found) sub ->
        match best_match ~after:i ~sub str with
        | Some (i, cost) -> i + String.length sub, found + cost, not_found
        | None -> i, found, not_found + String.length sub + 50)
      (0, 0, 0)
      words
  in
  found + not_found
