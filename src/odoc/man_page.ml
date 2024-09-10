open Odoc_document

let render _ ~sidebar:_ ~breadcrumbs:_ page = Odoc_manpage.Generator.render page

let filepath _ url = Odoc_manpage.Generator.filepath url

let renderer = { Renderer.name = "man"; render; filepath }
