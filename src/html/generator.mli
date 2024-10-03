open Odoc_document

val render :
  config:Config.t ->
  sidebar:Types.Block.t option ->
  breadcrumbs:Breadcrumbs.t ->
  Types.Document.t ->
  Renderer.page list

val filepath : config:Config.t -> Url.Path.t -> Fpath.t

val doc :
  config:Config.t ->
  xref_base_uri:string ->
  Types.Block.t ->
  Html_types.flow5_without_sectioning_heading_header_footer Tyxml.Html.elt list
