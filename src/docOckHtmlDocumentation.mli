open Tyxml.Html
open DocOck.Types.Documentation

val to_html : get_package:('a -> string) -> 'a t -> [> `Div | `P ] elt

val first_to_html : get_package:('a -> string) -> 'a t -> [> `Div | `P ] elt
(** Converts the first paragraph (i.e. everything up to the first blank line) to
    html. *)

val has_doc : 'a t -> bool
