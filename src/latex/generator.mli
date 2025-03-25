module Link : sig
  val label : Odoc_document.Url.t -> string
end

type config = {
  with_children : bool;
  shorten_beyond_depth : int option;
  remove_functor_arg_link : bool;
}

val render :
  config:config ->
  Odoc_document.Types.Document.t ->
  Odoc_document.Renderer.page list

val filepath : Odoc_document.Url.Path.t -> Fpath.t
