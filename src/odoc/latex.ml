open Odoc_document

type args = { with_children : bool }

let render args _sidebar page =
  Odoc_latex.Generator.render ~with_children:args.with_children page

let extra_documents _args _unit = []

let renderer = { Renderer.name = "latex"; render; extra_documents }
