(** Standard definition and types for all renderers *)

type syntax = OCaml | Reason

let string_of_syntax = function
  | OCaml -> "ml"
  | Reason -> "re"

type page = {
  filename : string;
  content : Format.formatter -> unit;
  children : page list
}

let traverse ~f t =
  let rec aux parents node =
    f ~parents node.filename node.content;
    List.iter (aux (node.filename :: parents)) node.children
  in
  aux [] t
