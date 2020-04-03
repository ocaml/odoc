open Odoc_document

val render :
  ?theme_uri:Tree.uri ->
  Types.Page.t -> Tree.t

val doc :
  xref_base_uri:string option ->
  Types.Block.t ->
  Html_types.flow5 Tyxml.Html.elt list
