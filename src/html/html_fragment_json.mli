module Html = Tyxml.Html

val make :
  config:Config.t ->
  preamble:Html_types.div_content Html.elt list ->
  url:Odoc_document.Url.Path.t ->
  breadcrumbs:Types.breadcrumb list ->
  toc:Odoc_document.Types.Toc.t ->
  uses_katex:bool ->
  Html_types.div_content Html.elt list ->
  Odoc_document.Renderer.page list ->
  Odoc_document.Renderer.page list
