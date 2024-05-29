val render :
  config:Config.t ->
  sidebar:Odoc_document.Types.Block.t option ->
  Odoc_document.Types.Document.t ->
  Odoc_document.Renderer.page list

val doc :
  config:Config.t ->
  xref_base_uri:string ->
  Odoc_document.Types.Block.t ->
  Html_types.flow5_without_sectioning_heading_header_footer Tyxml.Html.elt list
