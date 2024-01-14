module Entry = Db.Entry

type t =
  | Empty
  | Pq of Priority_queue.t
  | Inter of t * t
  | Union of t * t

let empty = Empty
let of_automata t = Pq (Priority_queue.of_automata t)
let of_array arr = Pq (Priority_queue.of_sorted_array (Some arr))

let inter a b =
  match a, b with
  | Empty, _ | _, Empty -> empty
  | x, y when x == y -> a
  | x, y -> Inter (x, y)

let union a b =
  match a, b with
  | Empty, _ -> b
  | _, Empty -> a
  | x, y when x == y -> a
  | x, y -> Union (x, y)

let rec join_with fn = function
  | [] -> []
  | [ x ] -> [ x ]
  | a :: b :: xs -> fn a b :: join_with fn xs

let rec perfect fn = function
  | [] -> Empty
  | [ x ] -> x
  | xs -> perfect fn (join_with fn xs)

let inter_of_list xs = perfect inter xs
let union_of_list xs = perfect union xs
let best x y = if Entry.compare x y <= 0 then x else y

let best_opt old_cand new_cand =
  match old_cand, new_cand with
  | None, None -> None
  | None, Some z | Some z, None -> Some z
  | Some x, Some y -> Some (best x y)

type strictness =
  | Gt
  | Ge

let rec succ ~strictness t elt =
  match t with
  | Empty -> None, t
  | Pq pqueue ->
    let pqueue =
      match strictness with
      | Gt -> Priority_queue.pop_lte elt pqueue
      | Ge -> Priority_queue.pop_lt elt pqueue
    in
    begin
      match Priority_queue.minimum pqueue with
      | None -> ()
      | Some e -> assert (Entry.compare elt e <= 0)
    end ;
    Priority_queue.minimum pqueue, Pq pqueue
  | Union (l, r) -> begin
    match succ ~strictness l elt with
    | None, _ -> succ ~strictness r elt
    | Some elt_l, l when strictness = Ge && Entry.equal elt elt_l -> Some elt, Union (l, r)
    | elt_l, l ->
      let elt_r, r = succ ~strictness r elt in
      best_opt elt_l elt_r, Union (l, r)
  end
  | Inter (l, r) ->
    let rec loop elt l r =
      match succ ~strictness:Ge l elt with
      | None, _ -> None, Empty
      | Some elt', l ->
        assert (Entry.compare elt elt' <= 0) ;
        if Entry.equal elt elt' then Some elt, Inter (l, r) else loop elt' r l
    in
    begin
      match succ ~strictness l elt with
      | None, _ -> None, Empty
      | Some elt_l, l -> loop elt_l r l
    end

let rec first t =
  match t with
  | Empty -> None, Empty
  | Pq pqueue -> Priority_queue.minimum pqueue, t
  | Inter (l, r) -> begin
    match first l with
    | None, _ -> None, Empty
    | Some elt, l -> succ ~strictness:Ge (Inter (l, r)) elt
  end
  | Union (l, r) ->
    let elt_l, l = first l in
    let elt_r, r = first r in
    best_opt elt_l elt_r, Union (l, r)

let seq_of_dispenser fn =
  let rec go () =
    match fn () with
    | None -> Seq.Nil
    | Some x -> Seq.Cons (x, go)
  in
  go

let to_seq t =
  let state = ref None in
  let loop () =
    let elt, t =
      match !state with
      | None -> first t
      | Some (previous_elt, t) -> succ ~strictness:Gt t previous_elt
    in
    match elt with
    | None -> None
    | Some elt ->
      state := Some (elt, t) ;
      Some elt
  in
  seq_of_dispenser loop
