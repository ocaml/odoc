open Odoc_document
open Or_error

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
  source_file:Fpath.t option ->
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
  source_file:Fpath.t option ->
  Fpath.t ->
  (unit, [> msg ]) result
