open Odoc_document
open Or_error

module Source : sig
  type t = File of Fpath.t | Root of Fpath.t

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string
end

type source = Source.t

val render_odoc :
  resolver:Resolver.t ->
  warnings_options:Odoc_model.Error.warnings_options ->
  syntax:Renderer.syntax ->
  renderer:'a Renderer.t ->
  output:Fs.directory ->
  'a ->
  Fpath.t ->
  (unit, [> msg ]) result

val generate_odoc :
  syntax:Renderer.syntax ->
  warnings_options:Odoc_model.Error.warnings_options ->
  renderer:'a Renderer.t ->
  output:Fs.directory ->
  extra_suffix:string option ->
  source:source option ->
  sidebar:Fpath.t option ->
  'a ->
  Fpath.t ->
  (unit, [> msg ]) result

val targets_odoc :
  resolver:Resolver.t ->
  warnings_options:Odoc_model.Error.warnings_options ->
  syntax:Renderer.syntax ->
  renderer:'a Renderer.t ->
  output:Fs.directory ->
  extra:'a ->
  source:source option ->
  Fpath.t ->
  (unit, [> msg ]) result
