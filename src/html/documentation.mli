module Html = Tyxml.Html

val to_html :
  ?wrap:unit -> Model.Comment.docs -> (Html_types.div_content Html.elt) list
(** When [wrap] is passed, then "(** *)" are added around the documentation. *)

val first_to_html : Model.Comment.docs -> (Html_types.div_content Html.elt) list
(** Converts the first paragraph (i.e. everything up to the first blank line) to
    html. *)

val has_doc : Model.Comment.docs -> bool
