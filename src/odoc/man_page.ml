open Odoc_document

let render _ page _ = [ Odoc_manpage.Generator.render page ]

let renderer = { Renderer.name = "man"; render }
