val html_of_doc :
  config:Config.t ->
  resolve:Link.resolve ->
  Odoc_document.Types.Source_page.code ->
  [> Html_types.pre ] Tyxml.Html.elt
