module Html = Tyxml.Html

val make :
  config:Config.t ->
  preamble:Html_types.div_content Html.elt list ->
  url:Odoc_document.Url.Path.t ->
  breadcrumbs:Types.breadcrumb list ->
  toc:Types.toc list ->
  uses_katex:bool ->
  source_anchor:string option ->
  Html_types.div_content Html.elt list ->
  Odoc_document.Renderer.page list ->
  Odoc_document.Renderer.page

val make_src :
  config:Config.t ->
  url:Odoc_document.Url.Path.t ->
  breadcrumbs:Types.breadcrumb list ->
  Html_types.div_content Html.elt list ->
  Odoc_document.Renderer.page
