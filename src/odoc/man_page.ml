open Odoc_document

let render _ page = Odoc_manpage.Generator.render page

let extra_documents _args _unit ~syntax:_ = []

let renderer = { Renderer.name = "man"; render; extra_documents }
