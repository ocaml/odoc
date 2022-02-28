
val render :
  ?theme_uri:Types.uri ->
  ?support_uri:Types.uri ->
  indent:bool ->
  Odoc_document.Types.Page.t ->
  Odoc_document.Renderer.page list

val doc :
  xref_base_uri:string ->
  Odoc_document.Types.Block.t ->
  Html_types.flow5_without_sectioning_heading_header_footer Tyxml.Html.elt list
