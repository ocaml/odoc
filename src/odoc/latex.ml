open Odoc_document

type args = Odoc_latex.Generator.config = {
  with_children : bool;
  shorten_beyond_depth : int option;
  remove_functor_arg_link : bool;
}

let render args _sidebar page = Odoc_latex.Generator.render ~config:args page

let filepath _args url = Odoc_latex.Generator.filepath url

let renderer = { Renderer.name = "latex"; render; filepath }
