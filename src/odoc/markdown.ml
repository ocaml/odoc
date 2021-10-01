open Odoc_document

type args = { generate_links : bool; md_flavour : string }

let render args (page : Odoc_document.Types.Page.t) :
    Odoc_document.Renderer.page =
  Odoc_markdown.Generator.args.md_flavour := args.md_flavour;
  Odoc_markdown.Generator.args.generate_links := args.generate_links;
  Odoc_markdown.Generator.render page

let files_of_url url = Odoc_markdown.Link.files_of_url url

let renderer = { Renderer.name = "markdown"; render; files_of_url }
