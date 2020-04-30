open Odoc_document

let render _ page = Odoc_manpage.Generator.render page

let renderer = {Renderer.
  render ;
}
