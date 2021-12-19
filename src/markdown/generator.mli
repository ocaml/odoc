type args = { generate_links : bool }

val render : Odoc_document.Types.Page.t -> args -> Odoc_document.Renderer.page
