type child = Page of string | Dir of string | Module of string

type short_title = Comment.link_content

type line

type children_order = child Location_.with_location list Location_.with_location

type t = {
  children_order : children_order option;
  short_title : short_title option;
  toc_status : [ `Open | `Hidden ] option;
  order_category : string option;
}

val empty : t

type tag_payload = Comment.nestable_block_element Location_.with_location list

val parse_children_order :
  Location_.span ->
  tag_payload ->
  (line Location_.with_location, Error.t) result

val parse_short_title :
  Location_.span ->
  tag_payload ->
  (line Location_.with_location, Error.t) result

val parse_toc_status :
  Location_.span ->
  tag_payload ->
  (line Location_.with_location, Error.t) result

val parse_order_category :
  Location_.span ->
  tag_payload ->
  (line Location_.with_location, Error.t) result

val of_lines : line Location_.with_location list -> t Error.with_warnings
