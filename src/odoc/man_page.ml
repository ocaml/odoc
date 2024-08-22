open Odoc_document

let render _ _sidebar page = Odoc_manpage.Generator.render page

let renderer = { Renderer.name = "man"; render }
