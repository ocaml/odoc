module Link : sig
  val label : Odoc_document.Url.t -> string
end

val render :
  with_children:bool ->
  Odoc_document.Types.Document.t ->
  Odoc_document.Renderer.page list
