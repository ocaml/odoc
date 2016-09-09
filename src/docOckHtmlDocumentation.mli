open Tyxml.Html
open DocOck.Types.Documentation

val to_html : get_package:('a -> string) -> 'a t -> [> `Div | `P ] elt

val first_to_html : get_package:('a -> string) -> 'a t -> [> `Div | `P ] elt

val has_doc : 'a t -> bool
