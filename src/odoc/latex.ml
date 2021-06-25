open Odoc_document

type args = { with_children : bool; flat : bool }

let render args page =
  Odoc_latex.Generator.render ~with_children:args.with_children page ~flat:args.flat

let renderer = { Renderer.name = "latex"; render }
