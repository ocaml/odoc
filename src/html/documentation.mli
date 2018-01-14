module Html = Tyxml.Html

val to_html :
  Model.Comment.docs ->
    (Html_types.flow5_without_header_footer Html.elt) list

val first_to_html :
  Model.Comment.docs -> (Html_types.flow5_without_header_footer Html.elt) list
(** Converts the first paragraph (i.e. everything up to the first blank line) to
    html. *)

val has_doc : Model.Comment.docs -> bool
