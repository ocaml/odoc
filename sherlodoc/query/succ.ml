module Entry = Db.Entry

type elt = Entry.t

type s =
  | Empty
  | All
  | Pq of Priority_queue.t
  | Inter of s * s
  | Union of s * s

type t =
  { s : s
  ; size : int
  }

let all = { s = All; size = 0 }
let empty = { s = Empty; size = 0 }
let make_pq t = { s = Pq t; size = Priority_queue.size t }
let of_automata t = make_pq (Priority_queue.of_automata t)
let of_automatas ts = make_pq Priority_queue.(of_list (List.map of_automata ts))
let of_array arr = make_pq (Priority_queue.of_sorted_array arr)

let inter a b =
  match a.s, b.s with
  | Empty, _ | _, Empty -> empty
  | _, All -> a
  | All, _ -> b
  | x, y when x == y -> a
  | x, y ->
    let s = if a.size <= b.size then Inter (x, y) else Inter (y, x) in
    { s; size = min a.size b.size }

let union a b =
  match a.s, b.s with
  | All, _ | _, All -> all
  | _, Empty -> a
  | Empty, _ -> b
  | x, y when x == y -> a
  | x, y ->
    let s = if a.size >= b.size then Union (x, y) else Union (y, x) in
    { s; size = a.size + b.size }

let rec join_with fn = function
  | [] -> []
  | [ x ] -> [ x ]
  | a :: b :: xs -> fn a b :: join_with fn xs

let rec perfect ~default fn = function
  | [] -> default
  | [ x ] -> x
  | xs -> perfect ~default fn (join_with fn xs)

let inter_of_list xs =
  let xs = List.sort (fun a b -> Int.compare a.size b.size) xs in
  perfect ~default:all inter xs

let union_of_list xs =
  let xs = List.sort (fun a b -> Int.compare b.size a.size) xs in
  perfect ~default:empty union xs

type strictness =
  | First
  | Ge of elt
  | Gt of elt

type result =
  | Is_empty
  | Is_all
  | Found_eq of s
  | Found_gt of elt * s

let rec succ ~strictness t =
  match t with
  | Empty -> Is_empty
  | All -> begin
    match strictness with
    | First -> Is_all
    | Gt _ -> Is_all
    | Ge _ -> Found_eq All
  end
  | Pq pqueue -> begin
    let pqueue' =
      match strictness with
      | First -> pqueue
      | Ge elt -> Priority_queue.pop_lt elt pqueue
      | Gt elt -> Priority_queue.pop_lte elt pqueue
    in
    match strictness, Priority_queue.minimum pqueue' with
    | _, None -> Is_empty
    | Ge elt, Some e when Db.Entry.equal e elt -> Found_eq (Pq pqueue')
    | _, Some e -> Found_gt (e, Pq pqueue')
  end
  | Union (l, r) -> begin
    match succ ~strictness l with
    | Is_empty -> succ ~strictness r
    | Is_all -> failwith "union all"
    | Found_eq l -> Found_eq (Union (l, r))
    | Found_gt (elt_l, l') -> begin
      match succ ~strictness r with
      | Is_empty -> Found_gt (elt_l, l')
      | Is_all -> failwith "union all"
      | Found_eq r' -> Found_eq (Union (l', r'))
      | Found_gt (elt_r, r') when Db.Entry.compare elt_l elt_r <= 0 ->
        Found_gt (elt_l, Union (l', r'))
      | Found_gt (elt_r, r') -> Found_gt (elt_r, Union (l', r'))
    end
  end
  | Inter (l, r) -> begin
    match succ ~strictness l with
    | Is_empty -> Is_empty
    | Is_all -> failwith "inter all"
    | Found_eq l' -> begin
      match succ ~strictness r with
      | Is_empty -> Is_empty
      | Is_all -> failwith "inter all"
      | Found_eq r' -> Found_eq (Inter (l', r'))
      | Found_gt (elt, r') -> Found_gt (elt, Inter (l', r'))
    end
    | Found_gt (elt, l') -> Found_gt (elt, Inter (l', r))
  end

let rec succ_loop ?(count = 0) ~strictness t =
  match strictness, succ ~strictness t with
  | _, Is_empty -> None
  | _, Is_all -> None
  | Ge elt, Found_eq t -> Some (elt, t)
  | _, Found_gt (elt, t) -> succ_loop ~count:(count + 1) ~strictness:(Ge elt) t
  | _ -> assert false

let first t = succ_loop ~strictness:First t

let seq_of_dispenser fn =
  let rec go () =
    match fn () with
    | None -> Seq.Nil
    | Some x -> Seq.Cons (x, go)
  in
  go

let to_seq { s = t; _ } =
  let state = ref None in
  let loop () =
    let result =
      match !state with
      | None -> first t
      | Some (previous_elt, t) -> succ_loop ~strictness:(Gt previous_elt) t
    in
    match result with
    | None -> None
    | Some (elt, _) ->
      state := result ;
      Some elt
  in
  seq_of_dispenser loop
