include List

let rec concat_map_sep ~sep ~f = function
  | [] -> []
  | [ x ] -> f x
  | x :: xs ->
      let hd = f x in
      let tl = concat_map_sep ~sep ~f xs in
      hd @ (sep :: tl)

let concat_map f l =
  let rec aux f acc = function
    | [] -> rev acc
    | x :: l ->
        let xs = f x in
        aux f (rev_append xs acc) l
  in
  aux f [] l

let rec filter_map acc f = function
  | hd :: tl ->
      let acc = match f hd with Some x -> x :: acc | None -> acc in
      filter_map acc f tl
  | [] -> List.rev acc

let filter_map f x = filter_map [] f x

(** @raise Failure if the list is empty. *)
let rec last = function
  | [] -> failwith "Odoc_utils.List.last"
  | [ x ] -> x
  | _ :: tl -> last tl

(* From ocaml/ocaml *)
let rec find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> find_map f l)

let rec find_opt p = function
  | [] -> None
  | x :: l -> if p x then Some x else find_opt p l

let is_empty = function [] -> true | _ :: _ -> false
