module Entry = Db.Entry

type node =
  | Empty
  | Pq of Priority_queue.t
  | Inter of node * node
  | Union of node * node

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
  | Union (l, r) ->
    let elt_l, l = succ ~strictness l elt in
    let elt_r, r = succ ~strictness r elt in
    best_opt elt_l elt_r, Union (l, r)
  | Inter (l, r) ->
    let rec loop elt l r =
      match succ ~strictness:Ge l elt with
      | None, _ -> None, Empty
      | Some elt_l, l -> begin
        match succ ~strictness:Ge r elt_l with
        | None, _ -> None, Empty
        | Some elt_r, r ->
          assert (Entry.compare elt_l elt_r <= 0) ;
          if Entry.compare elt_l elt_r = 0
          then Some elt_l, Inter (l, r)
          else loop elt_r l r
      end
    in
    begin
      match succ ~strictness l elt with
      | None, _ -> None, Empty
      | Some elt, l -> loop elt l r
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

type t =
  { cardinal : int
  ; s : node
  }

let to_seq { s; _ } =
  let state = ref None in
  let loop () =
    let elt, s =
      match !state with
      | None -> first s
      | Some (previous_elt, s) -> succ ~strictness:Gt s previous_elt
    in
    match elt with
    | None -> None
    | Some elt ->
      state := Some (elt, s) ;
      Some elt
  in
  Seq.of_dispenser loop

(** Functions to build a succ tree *)

let empty = { cardinal = 0; s = Empty }

let inter a b =
  match a.s, b.s with
  | Empty, _ | _, Empty -> empty
  | x, y when x == y -> a
  | x, y ->
    let x, y = if a.cardinal < b.cardinal then x, y else y, x in
    { cardinal = min a.cardinal b.cardinal; s = Inter (x, y) }

let union a b =
  match a.s, b.s with
  | Empty, _ -> b
  | _, Empty -> a
  | x, y when x == y -> a
  | x, y ->
    let x, y = if a.cardinal < b.cardinal then x, y else y, x in
    { cardinal = a.cardinal + b.cardinal; s = Union (x, y) }

let inter_of_list = function
  | [] -> empty
  | elt :: li -> List.fold_left inter elt li

let of_automata t = { s = Pq (Priority_queue.of_automata t); cardinal = 1 }

let of_array arr =
  { s = Pq (Priority_queue.of_sorted_array (Some arr)); cardinal = Array.length arr }
