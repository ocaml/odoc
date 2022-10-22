open Db.Types

type s =
  | All
  | Empty
  | Set of Elt_set.t
  | Inter of s * s
  | Union of s * s

type t =
  { cardinal : int
  ; s : s
  }

let all = { cardinal = -1; s = All }
let empty = { cardinal = 0; s = Empty }

let of_set s =
  if Elt_set.is_empty s
  then empty
  else { cardinal = Elt_set.cardinal s; s = Set s }

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
  | x, y -> { cardinal = a.cardinal + b.cardinal; s = Union (x, y) }

let succ_ge' elt set = Elt_set.find_first (fun e -> Elt.compare e elt >= 0) set
let succ_gt' elt set = Elt_set.find_first (fun e -> Elt.compare e elt > 0) set
let first' set = Elt_set.find_first (fun _ -> true) set

exception Gt of Elt.t

let rec succ_ge elt = function
  | All -> elt
  | Empty -> raise Not_found
  | Set s ->
      let out = succ_ge' elt s in
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
  | Set s -> succ_gt' elt s
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
  | Set s -> first' s
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

let first t =
  let rec go n elt acc =
    if n <= 0
    then Lwt.return (List.rev acc)
    else
      let open Lwt.Syntax in
      let* () = Lwt.pause () in
      match succ_ge elt t with
      | elt' ->
          assert (Elt.compare elt elt' = 0) ;
          go_gt (n - 1) elt (elt :: acc)
      | exception Gt elt -> go n elt acc
      | exception Not_found -> Lwt.return (List.rev acc)
  and go_gt n elt acc =
    match succ_gt elt t with
    | elt -> go n elt acc
    | exception Not_found -> Lwt.return (List.rev acc)
  in
  Lwt.catch (fun () -> go 100 (first t) []) (fun (_ : exn) -> Lwt.return [])

let to_list t = first t.s
