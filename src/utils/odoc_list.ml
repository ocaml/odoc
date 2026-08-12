let rec concat_map_sep ~sep ~f = function
  | [] -> []
  | [ x ] -> f x
  | x :: xs ->
      let hd = f x in
      let tl = concat_map_sep ~sep ~f xs in
      hd @ (sep :: tl)

(** @raise Failure if the list is empty. *)
let rec last = function
  | [] -> failwith "Odoc_utils.List.last"
  | [ x ] -> x
  | _ :: tl -> last tl

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

module Overlay = struct
  module Either = Odoc_either

  (* Since 4.12. Copied from ocaml/ocaml *)
  let partition_map p l =
    let rec part left right = function
      | [] -> (List.rev left, List.rev right)
      | x :: l -> (
          match p x with
          | Either.Left v -> part (v :: left) right l
          | Either.Right v -> part left (v :: right) l)
    in
    part [] [] l

  (* Since 5.1 *)
  let is_empty = function [] -> true | _ :: _ -> false

  (* Since 4.10. Copied ocaml/ocaml *)
  let rec find_map f = function
    | [] -> None
    | x :: l -> (
        match f x with Some _ as result -> result | None -> find_map f l)

  (* Since 4.10 *)
  let concat_map f l =
    let rec aux f acc = function
      | [] -> List.rev acc
      | x :: l ->
          let xs = f x in
          aux f (List.rev_append xs acc) l
    in
    aux f [] l

  (* shadow the functions defined above with the stdlib variants if available *)
  include List
end

include Overlay
