open Odoc_document

type args = { generate_links : bool }

let render (args : args) (page : Odoc_document.Types.Page.t) :
    Odoc_document.Renderer.page =
  Odoc_markdown.Generator.render page { generate_links = args.generate_links }

let files_of_url url = Odoc_markdown.Link.files_of_url url

let renderer = { Renderer.name = "markdown"; render; files_of_url }
