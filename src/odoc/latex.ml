open Odoc_document

type args = { with_children : bool; flat : bool; extra_suffix : string }

let render args page =
  Odoc_latex.Generator.render ~with_children:args.with_children page ~flat:args.flat ~extra_suffix:args.extra_suffix

let renderer = { Renderer.name = "latex"; render }
