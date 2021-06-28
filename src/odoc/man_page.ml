open Odoc_document

type args = { flat : bool; extra_suffix : string }

let render args page =
  Odoc_manpage.Generator.render page ~flat:args.flat ~extra_suffix:args.extra_suffix

let renderer = { Renderer.name = "man"; render}
