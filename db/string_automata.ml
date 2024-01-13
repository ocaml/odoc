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
    then None
    else begin
      let node = arr.(i) in
      if chr = str.[node.start - 1] then Some node else go (i + 1)
    end
  in
  go 0

let array_find ~str chr = function
  | None -> None
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
  then Some node
  else begin
    match array_find ~str pattern.[i] node.children with
    | None -> None
    | Some child -> find_lcp ~str child pattern (i + 1)
  end

and find_lcp ~str child pattern i =
  let n = lcp pattern i str child.start child.len in
  if i + n = String.length pattern
  then Some { child with start = child.start + n; len = child.len - n }
  else if n = child.len
  then find ~str child pattern (i + n)
  else None

let find t pattern =
  match find_lcp ~str:t.str t.t pattern 0 with
  | None -> None
  | Some child -> Some { str = t.str; t = child }

let advance node =
  assert (node.len >= 1) ;
  { node with start = node.start + 1; len = node.len - 1 }

let stepback node =
  assert (node.len >= 0) ;
  { node with start = node.start - 1; len = node.len + 1 }

let rec find_skip ~spaces t pattern =
  let skip () =
    let node = t.t in
    if node.len >= 1
    then begin
      let spaces = spaces + if t.str.[node.start] = ' ' then 1 else 0 in
      if spaces > 1 then [] else find_skip ~spaces { t with t = advance t.t } pattern
    end
    else begin
      match node.children with
      | None -> []
      | Some children ->
        snd
        @@ List.fold_left
             (fun (i, acc) child ->
               let xs = find_skip ~spaces { t with t = stepback child } pattern in
               i + 1, List.rev_append xs acc)
             (0, [])
        @@ Array.to_list children
    end
  in
  if spaces = 0
  then skip ()
  else begin
    let skip = skip () in
    match find t pattern with
    | Some here -> here :: skip
    | None -> skip
  end

let find_star t pattern =
  let rec go t = function
    | [] -> [ t ]
    | p :: ps -> begin
      let ts = find_skip ~spaces:0 t p in
      List.fold_left
        (fun acc t ->
          let xs = go t ps in
          List.rev_append xs acc)
        []
        ts
    end
  in
  match String.split_on_char ' ' pattern with
  | [] -> []
  | p :: ps -> begin
    match find t p with
    | None -> []
    | Some t -> go t ps
  end

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
