type child = Page of string | Dir of string

type line

type children_order = child Location_.with_location list Location_.with_location

type t = { children_order : children_order option }

val empty : t

val parse_children_order :
  Location_.span ->
  Odoc_parser.Ast.nestable_block_element Location_.with_location list ->
  (line Location_.with_location, Error.t) result

val of_lines : line Location_.with_location list -> t
