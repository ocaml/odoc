include List

let rec concat_map_sep ~sep ~f = function
  | [] -> []
  | [ x ] -> f x
  | x :: xs ->
      let hd = f x in
      let tl = concat_map_sep ~sep ~f xs in
      hd @ (sep :: tl)

(* Since 4.10 *)
let concat_map f l =
  let rec aux f acc = function
    | [] -> rev acc
    | x :: l ->
        let xs = f x in
        aux f (rev_append xs acc) l
  in
  aux f [] l

(** @raise Failure if the list is empty. *)
let rec last = function
  | [] -> failwith "Odoc_utils.List.last"
  | [ x ] -> x
  | _ :: tl -> last tl

(* Since 4.10. Copied ocaml/ocaml *)
let rec find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> find_map f l)

(* Since 5.1 *)
let is_empty = function [] -> true | _ :: _ -> false

let rec skip_until ~p = function
  | [] -> []
  | h :: t -> if p h then t else skip_until ~p t

let split_at ~f lst =
  let rec loop acc = function
    | hd :: _ as rest when f hd -> (List.rev acc, rest)
    | [] -> (List.rev acc, [])
    | hd :: tl -> loop (hd :: acc) tl
  in
  loop [] lst
