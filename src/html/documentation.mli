module Html = Tyxml.Html

val to_html :
  ?wrap:unit ->
  (Model.Comment.comment', Model.Error.t) result ->
    (Html_types.div_content Html.elt) list
(** When [wrap] is passed, then "(** *)" are added around the documentation. *)

val first_to_html :
  (Model.Comment.comment', Model.Error.t) result ->
    (Html_types.div_content Html.elt) list
(** Converts the first paragraph (i.e. everything up to the first blank line) to
    html. *)

val has_doc : (Model.Comment.comment', Model.Error.t) result -> bool
