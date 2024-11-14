type child = Page of string | Dir of string

type short_title = Comment.link_content

type line

type children_order = child Location_.with_location list Location_.with_location

type t = {
  children_order : children_order option;
  short_title : short_title option;
}

val empty : t

val parse_children_order :
  Location_.span ->
  Comment.nestable_block_element Location_.with_location list ->
  (line Location_.with_location, Error.t) Result.result

val parse_short_title :
  Location_.span ->
  Comment.nestable_block_element Location_.with_location list ->
  (line Location_.with_location, Error.t) Result.result

val of_lines : line Location_.with_location list -> t Error.with_warnings
