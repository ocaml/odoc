open Odoc_model

val constructor : Paths.Identifier.t -> 'b -> 'c -> string
val type_expr : ?needs_parentheses:bool -> Lang.TypeExpr.t -> string
val html_of_entry : Fold.item -> Html_types.div Tyxml.Html.elt
val html_of_doc : Comment.docs -> Html_types.div Tyxml.Html.elt
