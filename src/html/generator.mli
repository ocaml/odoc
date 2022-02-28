open Odoc_document

val render :
  ?theme_uri:Tree.uri ->
  ?support_uri:Tree.uri ->
  indent:bool ->
  Types.Page.t ->
  Renderer.page list

val doc :
  xref_base_uri:string ->
  Types.Block.t ->
  Html_types.flow5_without_sectioning_heading_header_footer Tyxml.Html.elt list
