open Db


type s =
  | All
  | Empty
  | Array of Elt.t array
  | Inter of s * s
  | Union of s * s

type t =
  { cardinal : int
  ; s : s
  }

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
  | x, y ->
      let x, y = if a.cardinal < b.cardinal then x, y else y, x in
      { cardinal = min a.cardinal b.cardinal; s = Inter (x, y) }

let union a b =
  match a.s, b.s with
  | Empty, _ -> b
  | _, Empty -> a
  | All, _ | _, All -> all
  | x, y ->
      let x, y = if a.cardinal < b.cardinal then x, y else y, x in
      { cardinal = a.cardinal + b.cardinal; s = Union (x, y) }

let array_first arr = arr.(0)

exception Gt of Elt.t

let rec succ_ge elt = function
  | All -> elt
  | Empty -> raise Not_found
  | Array s ->
      let out = Array.succ_ge_exn ~compare:Elt.compare elt s in
      begin
        match Elt.compare elt out with
        | 0 -> elt
        | _ -> raise (Gt out)
      end
  | Inter (a, b) ->
      let _ = succ_ge elt a in
      let y = succ_ge elt b in
      y
  | Union (a, b) -> begin
      match succ_ge elt a with
      | exception Not_found -> succ_ge elt b
      | exception Gt x -> begin
          match succ_ge elt b with
          | exception Not_found -> raise (Gt x)
          | exception Gt y ->
              raise
                (Gt
                   (match Elt.compare x y with
                   | c when c <= 0 -> x
                   | _ -> y))
          | v -> v
        end
      | v -> v
    end

let rec succ_gt elt = function
  | All -> invalid_arg "Succ.succ_gt All"
  | Empty -> raise Not_found
  | Array s -> Array.succ_gt_exn ~compare:Elt.compare elt s
  | Inter (a, _b) -> succ_gt elt a
  | Union (a, b) -> begin
      match succ_gt_opt elt a, succ_gt_opt elt b with
      | None, None -> raise Not_found
      | None, Some z | Some z, None -> z
      | Some x, Some y -> begin
          match Elt.compare x y with
          | c when c <= 0 -> x
          | _ -> y
        end
    end

and succ_gt_opt elt t = try Some (succ_gt elt t) with Not_found -> None

let rec first = function
  | All -> invalid_arg "Succ.first All"
  | Empty -> raise Not_found
  | Array s -> array_first s
  | Inter (a, _b) -> first a
  | Union (a, b) -> begin
      match first_opt a, first_opt b with
      | None, None -> raise Not_found
      | None, Some z | Some z, None -> z
      | Some x, Some y -> begin
          match Elt.compare x y with
          | 0 -> x
          | c when c < 0 -> x
          | _ -> y
        end
    end

and first_opt t = try Some (first t) with Not_found -> None

let to_seq t =
  let state = ref None in
  let rec go elt =
    match succ_ge elt t with
    | elt' ->
        assert (Elt.compare elt elt' = 0) ;
        state := Some elt ;
        Some elt
    | exception Gt elt -> go elt
    | exception Not_found -> None
  in
  let go_gt () =
    match !state with
    | None -> go (first t)
    | Some previous_elt -> (
        match succ_gt previous_elt t with
        | elt -> go elt
        | exception Not_found -> None)
  in
  let next () = try go_gt () with _ -> None in
  Seq.of_dispenser next

let to_seq t = to_seq t.s
