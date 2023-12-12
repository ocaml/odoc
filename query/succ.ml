type 'a node =
  | All
  | Empty
  | Array of 'a array
  | Inter of 'a node * 'a node
  | Union of 'a node * 'a node

let rec print_node a ~depth s =
  print_string (String.make (depth * 4) ' ') ;
  let depth = depth + 1 in
  match s with
  | All -> print_endline "All"
  | Empty -> print_endline "Empty"
  | Inter (l, r) ->
      print_endline "Inter" ;
      print_node a ~depth l ;
      print_node a ~depth r
  | Union (l, r) ->
      print_endline "Union" ;
      print_node a ~depth l ;
      print_node a ~depth r
  | Array arr ->
      print_string "{ " ;
      Array.iter
        (fun elt ->
          a elt ;
          print_string " ")
        arr ;
      print_endline "}"

let print_node a s = print_node a ~depth:0 s

let best ~compare x y =
  match compare x y with
  | 0 -> x
  | c when c < 0 -> x
  | _ -> y

let best_opt ~compare old_cand new_cand =
  match old_cand, new_cand with
  | None, None -> None
  | None, Some z | Some z, None -> Some z
  | Some x, Some y -> Some (best ~compare x y)

let ( let* ) = Option.bind

type strictness =
  | Gt
  | Ge

let array_succ ~strictness =
  match strictness with
  | Ge -> Array_succ.succ_ge
  | Gt -> Array_succ.succ_gt

let rec succ ~compare ~strictness t elt =
  match t with
  | All -> invalid_arg "Succ.succ_rec All"
  | Empty -> None
  | Array arr -> array_succ ~strictness ~compare elt arr
  | Union (l, r) ->
      let elt_r = succ ~compare ~strictness r elt in
      let elt_l = succ ~compare ~strictness l elt in
      best_opt ~compare elt_l elt_r
  | Inter (l, r) ->
      let rec loop elt_r =
        let* elt_l = succ ~compare ~strictness:Ge l elt_r in
        let* elt_r = succ ~compare ~strictness:Ge r elt_l in
        if compare elt_l elt_r = 0 then Some elt_l else loop elt_r
      in
      let* elt_l = succ ~compare ~strictness l elt in
      loop elt_l

let rec first ~compare t =
  match t with
  | All -> invalid_arg "Succ.first All"
  | Empty -> None
  | Array s -> Some s.(0)
  | Inter (l, _) ->
      let* elt = first ~compare l in
      succ ~strictness:Ge ~compare t elt
  | Union (l, r) -> begin
      let elt_l = first ~compare l in
      let elt_r = first ~compare r in
      best_opt ~compare elt_l elt_r
    end

type 'a t =
  { cardinal : int
  ; s : 'a node
  }

let to_seq ~compare { s; _ } =
  let state = ref None in
  let loop () =
    let elt =
      match !state with
      | None -> first ~compare s
      | Some previous_elt -> succ ~strictness:Gt ~compare s previous_elt
    in
    state := elt ;
    elt
  in
  (* Here, as stackoverflow could be thrown. In that case, we do not want to
     crash, as a more complex search will have fewer results and probably not
     trigger the stackoverflow, and we want the webworker or server to be
     running when such a request is inputed.
     The Printexc is very important as we need to be able to tell if the
     situation described above happens.
     With the current algorithm, such a stackoverflow is never triggered even
     on big libraries like Base, but it is not tail-rec, so a big enough search
     db could trigger it. *)
  let next () = try Printexc.print loop () with _ -> None in
  Seq.of_dispenser next

(** Functions to build a succ tree *)

let all = { cardinal = -1; s = All }
let empty = { cardinal = 0; s = Empty }

let of_array arr =
  if Array.length arr = 0
  then empty
  else { cardinal = Array.length arr; s = Array arr }

let inter a b =
  match a.s, b.s with
  | Empty, _ | _, Empty -> empty
  | _, All -> a
  | All, _ -> b
  | x, y when x == y -> a
  | x, y ->
      let x, y = if a.cardinal < b.cardinal then x, y else y, x in
      { cardinal = min a.cardinal b.cardinal; s = Inter (x, y) }

let union a b =
  match a.s, b.s with
  | Empty, _ -> b
  | _, Empty -> a
  | All, _ | _, All -> all
  | x, y when x == y -> a
  | x, y ->
      let x, y = if a.cardinal < b.cardinal then x, y else y, x in
      { cardinal = a.cardinal + b.cardinal; s = Union (x, y) }

(** This does a dychotomy to avoid building a comb, which would have poor
    performance. *)
let union_of_array arr =
  let rec loop lo hi =
    match hi - lo with
    | 0 -> empty
    | 1 -> arr.(lo)
    | dist ->
        let mid = lo + (dist / 2) in
        let left = loop lo mid in
        let right = loop mid hi in
        union left right
  in
  loop 0 (Array.length arr)

let union_of_list li = li |> Array.of_list |> union_of_array
let print a { s; _ } = print_node a s
