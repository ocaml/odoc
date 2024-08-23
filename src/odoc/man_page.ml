open Odoc_document

let render _ _sidebar page = Odoc_manpage.Generator.render page

let filepath _ url = Odoc_manpage.Generator.filepath url

let renderer = { Renderer.name = "man"; render; filepath }
