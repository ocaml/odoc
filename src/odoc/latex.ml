open Odoc_document

type args = { with_children : bool }

let render args page =
  Odoc_latex.Generator.render ~with_children:args.with_children page

let files_of_url url = Odoc_latex.Generator.files_of_url url

let renderer = { Renderer.name = "latex"; render; files_of_url }
