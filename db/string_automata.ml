type terminals =
  | Empty
  | Terminals of Entry.t array
  | Summary of Entry.t array

type node =
  { start : int
  ; len : int
  ; size : int
  ; terminals : terminals
  ; children : node array option
  }

type t =
  { str : string
  ; t : node
  }

let empty = { start = 0; len = 0; size = 0; children = None; terminals = Empty }

let empty () =
  (* avoid ancient segfaulting on statically allocated values *)
  Obj.obj @@ Obj.dup @@ Obj.repr empty

let size t = t.t.size

let minimum { t; _ } =
  match t.terminals with
  | Empty -> assert false
  | Terminals arr | Summary arr -> arr.(0)

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
    else begin
      let i_chr, j_chr = i_str.[i], j_str.[j] in
      if i_chr <> j_chr then i else go_lcp (i + 1) (j + 1)
    end
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

let rec find_skip ~spaces t pattern yield =
  let skip () =
    let node = t.t in
    if node.len >= 1
    then begin
      let spaces = spaces + if t.str.[node.start] = ' ' then 1 else 0 in
      if spaces > 1
      then ()
      else find_skip ~spaces { t with t = advance t.t } pattern yield
    end
    else begin
      match node.children with
      | None -> ()
      | Some children ->
        Array.iter
          (fun child -> find_skip ~spaces { t with t = stepback child } pattern yield)
          children
    end
  in
  if spaces = 0
  then skip ()
  else if spaces = 1 && pattern = Type_polarity.poly
  then begin
    match find t pattern with
    | None -> ()
    | Some here -> yield here
  end
  else begin
    skip () ;
    match find t pattern with
    | None -> ()
    | Some here -> yield here
  end

let find_star t pattern yield =
  let rec go t = function
    | [] -> yield t
    | p :: ps -> find_skip ~spaces:0 t p @@ fun t -> go t ps
  in
  match String.split_on_char ' ' pattern with
  | [] -> ()
  | p :: ps -> begin
    match find t p with
    | None -> ()
    | Some t -> go t ps
  end

let find_star t pattern =
  let found = ref [] in
  find_star t pattern (fun t -> found := t :: !found) ;
  !found
