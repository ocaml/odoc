open Odoc_document

let render _ page = Odoc_mli.Generator.render page

let files_of_url url = Odoc_mli.Link.files_of_url url

let renderer = { Renderer.name = "mli"; render; files_of_url }
