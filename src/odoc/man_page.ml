open Odoc_document

let render _ page = Odoc_manpage.Generator.render page

let files_of_url url = Odoc_manpage.Link.files_of_url url

let renderer = {Renderer.
  name = "man" ;
  render ;
  files_of_url ;
}
