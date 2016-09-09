open Tyxml.Html
open DocOck.Types.Documentation

val to_html : 'a t -> [> `Div | `P ] elt

val first_to_html : 'a t -> [> `Div | `P ] elt

val has_doc : 'a t -> bool
