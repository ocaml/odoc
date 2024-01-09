type node =
  { start : int
  ; len : int
  ; terminals : Entry.Array.t
  ; children : node array option
  }

type t =
  { str : string
  ; t : node
  }

let array_find ~str chr arr =
  let rec go i =
    if i >= Array.length arr
    then raise Not_found
    else begin
      let node = arr.(i) in
      if chr = str.[node.start - 1] then node else go (i + 1)
    end
  in
  go 0

let array_find ~str chr = function
  | None -> raise Not_found
  | Some arr -> array_find ~str chr arr

let lcp i_str i j_str j j_len =
  let j_stop = j + j_len in
  let rec go_lcp i j =
    if i >= String.length i_str || j >= j_stop
    then i
    else (
      let i_chr, j_chr = i_str.[i], j_str.[j] in
      if i_chr <> j_chr then i else go_lcp (i + 1) (j + 1))
  in
  let i' = go_lcp i j in
  i' - i

let rec find ~str node pattern i =
  if i >= String.length pattern
  then node
  else (
    let chr = pattern.[i] in
    let child = array_find ~str chr node.children in
    find_lcp ~str child pattern (i + 1))

and find_lcp ~str child pattern i =
  let n = lcp pattern i str child.start child.len in
  if i + n = String.length pattern
  then { child with start = child.start + n }
  else if n = child.len
  then find ~str child pattern (i + n)
  else raise Not_found

let find t pattern =
  let child = find ~str:t.str t.t pattern 0 in
  { str = t.str; t = child }

let find t pattern =
  try Some (find t pattern) with
  | Not_found -> None

let min_opt a b =
  match a, b with
  | Some x, Some y -> Some (if Entry.compare x y <= 0 then x else y)
  | Some x, None | None, Some x -> Some x
  | None, None -> None

let rec minimum t =
  let min_terminal =
    match t.terminals with
    | None -> None
    | Some arr -> Some arr.(0)
  in
  let min_child =
    match t.children with
    | None -> None
    | Some children -> minimum children.(0)
  in
  min_opt min_terminal min_child

let minimum { t; _ } =
  match minimum t with
  | None -> assert false
  | Some elt -> elt
