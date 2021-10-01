type args = { generate_links : bool ref; md_flavour : string ref }

val args : args

val render : Odoc_document.Types.Page.t -> Odoc_document.Renderer.page
