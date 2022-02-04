open Odoc_document

type args = { generate_links : bool }

let render { generate_links } (page : Odoc_document.Types.Page.t) :
    Odoc_document.Renderer.page =
  Odoc_markdown.Generator.render ~generate_links page

let files_of_url url = Odoc_markdown.Link.files_of_url url

let renderer = { Renderer.name = "markdown"; render; files_of_url }
