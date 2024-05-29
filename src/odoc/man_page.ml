open Odoc_document

let render _ _sidebar page = Odoc_manpage.Generator.render page

let extra_documents _args _unit = []

let renderer = { Renderer.name = "man"; render; extra_documents }
