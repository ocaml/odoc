module String_automata = Db.String_automata
module Entry = Db.Entry

type elt = Entry.t

type t =
  | Empty
  | Array of int * elt array
  | All of elt * String_automata.t
  | Union of elt * t list

let minimum = function
  | Empty -> None
  | Array (i, arr) -> Some arr.(i)
  | All (elt, _) | Union (elt, _) -> Some elt

let of_sorted_array = function
  | None -> Empty
  | Some arr -> Array (0, arr)

let of_automata s =
  let elt = String_automata.minimum s in
  All (elt, s)

let insert_sort x lst =
  match minimum x with
  | None -> lst
  | Some min_elt ->
    let rec go lst =
      match lst with
      | [] -> [ x ]
      | y :: ys -> begin
        match minimum y with
        | None -> go ys
        | Some min_y when Entry.compare min_elt min_y <= 0 -> x :: lst
        | _ -> y :: go ys
      end
    in
    go lst

let union_with ~min_elt lst =
  match List.filter (( <> ) Empty) lst with
  | [] -> Empty
  | [ t ] -> t
  | sorted_lst -> Union (min_elt, sorted_lst)

let rec union_sorted lst =
  match lst with
  | [] -> Empty
  | [ t ] -> t
  | x :: xs -> begin
    match minimum x with
    | None -> union_sorted xs
    | Some min_elt -> Union (min_elt, lst)
  end

let rec pop_until cond = function
  | Empty -> Empty
  | Array (i, arr) as t ->
    let rec search i j =
      assert (not (cond arr.(i))) ;
      assert (cond arr.(j)) ;
      let m = (i + j) / 2 in
      if i = m then Array (j, arr) else if cond arr.(m) then search i m else search m j
    in
    let rec go j step =
      if j >= Array.length arr
      then begin
        let last = Array.length arr - 1 in
        let j_prev = j - (step / 2) in
        if cond arr.(last) then search j_prev last else Empty
      end
      else if cond arr.(j)
      then if i = j then t else search (j - (step / 2)) j
      else go (j + step) (step * 2)
    in
    go i 1
  | All (min_elt, _) as t when cond min_elt -> t
  | All (min_elt, ({ String_automata.t; _ } as automata)) ->
    let terminals = of_sorted_array t.terminals in
    let children =
      Array.to_list
      @@ Array.map (fun child -> of_automata { automata with t = child })
      @@ Option.value ~default:[||] t.children
    in
    let all = insert_sort terminals children in
    pop_until cond (union_with ~min_elt all)
  | Union (min_elt, _) as t when cond min_elt -> t
  | Union (_, lst) ->
    let rec go = function
      | [] -> []
      | x :: xs ->
        let x' = pop_until cond x in
        if x == x' then x :: xs else insert_sort x' (go xs)
    in
    let lst = go lst in
    union_sorted lst

let pop_lt elt t = pop_until (fun x -> Entry.compare x elt >= 0) t
let pop_lte elt t = pop_until (fun x -> Entry.compare x elt > 0) t
