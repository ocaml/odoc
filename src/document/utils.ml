
let rec flatmap ?sep ~f = function
  | [] -> []
  | [x] -> f x
  | x :: xs ->
    let hd = f x in
    let tl = flatmap ?sep ~f xs in
    match sep with
    | None -> hd @ tl
    | Some sep -> hd @ sep @ tl
