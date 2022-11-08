open Odoc_document

let render _ page = [ Odoc_manpage.Generator.render page ]

let renderer =
  { Renderer.name = "man"; render; render_src = (fun _ _ _ _ -> []) }
