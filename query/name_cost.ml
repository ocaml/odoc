let rec is_prefix_at ~sub i s j =
  if i >= String.length sub
  then true
  else if sub.[i] = s.[j]
  then is_prefix_at ~sub (i + 1) s (j + 1)
  else false

let is_substring ~sub s =
  let rec go j =
    if j + String.length sub > String.length s
    then false
    else if is_prefix_at ~sub 0 s j
    then true
    else go (j + 1)
  in
  go 0

let starts_with ~prefix str =
  String.length prefix <= String.length str && is_prefix_at ~sub:prefix 0 str 0

let ends_with ~suffix str =
  let j = String.length str - String.length suffix in
  j >= 0 && is_prefix_at ~sub:suffix 0 str j
