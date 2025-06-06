(** Supported languages for printing code parts. *)

(** {1 Page creator} *)

val make :
  config:Config.t ->
  url:Odoc_document.Url.Path.t ->
  Renderer.doc ->
  Odoc_document.Renderer.page list ->
  Odoc_document.Renderer.page

val make_src :
  config:Config.t ->
  url:Odoc_document.Url.Path.t ->
  string ->
  Renderer.Block.t list ->
  Odoc_document.Renderer.page
