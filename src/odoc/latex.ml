open Odoc_document

type args = { with_children : bool }

let render args _sidebar page =
  Odoc_latex.Generator.render ~with_children:args.with_children page

let filepath _args url = Odoc_latex.Generator.filepath url

let renderer = { Renderer.name = "latex"; render; filepath }
