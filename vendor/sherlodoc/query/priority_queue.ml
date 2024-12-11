module String_automata = Db.String_automata
module Entry = Db.Entry

type elt = Entry.t

type t =
  | Empty
  | Array of int * elt array
  | All of elt * String_automata.t
  | Union of elt * t list

let rec size = function
  | Empty -> 0
  | Array (i, arr) -> Array.length arr - i
  | All (_, s) -> String_automata.size s
  | Union (_, xs) -> List.fold_left (fun acc x -> acc + size x) 0 xs

let minimum = function
  | Empty -> None
  | Array (i, arr) -> Some arr.(i)
  | All (elt, _) | Union (elt, _) -> Some elt

let of_sorted_array arr = Array (0, arr)

let of_automata s =
  let elt = String_automata.minimum s in
  All (elt, s)

let of_list lst =
  let lst = List.filter (( <> ) Empty) lst in
  let min x =
    match minimum x with
    | None -> assert false
    | Some elt -> elt
  in
  let compare a b = Entry.compare (min a) (min b) in
  match List.sort compare lst with
  | [] -> Empty
  | hd :: _ as lst -> Union (min hd, lst)

let insert_sort x lst =
  match minimum x with
  | None -> lst
  | Some min_elt ->
    let rec insert lst =
      match lst with
      | [] -> [ x ]
      | y :: ys -> begin
        match minimum y with
        | None -> insert ys
        | Some min_y when Entry.compare min_elt min_y <= 0 -> x :: lst
        | _ -> y :: insert ys
      end
    in
    insert lst

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

let expand_automata ~min_elt ({ String_automata.t; _ } as automata) =
  match t.terminals with
  | String_automata.Summary arr -> Array (0, arr)
  | terminals ->
    let terminals =
      match terminals with
      | String_automata.Empty -> Empty
      | Terminals terminals -> Array (0, terminals)
      | _ -> assert false
    in
    let lift child = of_automata { automata with String_automata.t = child } in
    let children =
      Array.to_list @@ Array.map lift @@ Option.value ~default:[||] t.children
    in
    let all = insert_sort terminals children in
    union_with ~min_elt all

let rec pop_until cond = function
  | Empty -> Empty
  | Array (i, arr) as t ->
    let rec search i j =
      assert (not (cond arr.(i))) ;
      assert (cond arr.(j)) ;
      let m = (i + j) / 2 in
      if i = m then Array (j, arr) else if cond arr.(m) then search i m else search m j
    in
    let rec search_from j step =
      if j >= Array.length arr
      then begin
        let last = Array.length arr - 1 in
        let j_prev = j - (step / 2) in
        if cond arr.(last) then search j_prev last else Empty
      end
      else if cond arr.(j)
      then if i = j then t else search (j - (step / 2)) j
      else search_from (j + step) (step * 2)
    in
    search_from i 1
  | All (min_elt, _) as t when cond min_elt -> t
  | All (min_elt, automata) -> pop_until cond (expand_automata ~min_elt automata)
  | Union (min_elt, _) as t when cond min_elt -> t
  | Union (_, lst) ->
    let rec pop_union i = function
      | [] -> []
      | x :: xs ->
        let x' = pop_until cond x in
        if x == x'
        then begin
          assert (i > 0) ;
          x :: xs
        end
        else insert_sort x' (pop_union (i + 1) xs)
    in
    let lst = pop_union 0 lst in
    union_sorted lst

let pop_lt elt t =
  let cmp_lt x = Entry.compare x elt >= 0 in
  pop_until cmp_lt t

let pop_lte elt t =
  let cmp_lte x = Entry.compare x elt > 0 in
  pop_until cmp_lte t
