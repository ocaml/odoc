val render :
  config:Config.t ->
  Odoc_document.Types.Page.t ->
  Odoc_document.Renderer.page list

val doc :
  config:Config.t ->
  xref_base_uri:string ->
  Odoc_document.Types.Block.t ->
  Html_types.flow5_without_sectioning_heading_header_footer Tyxml.Html.elt list
