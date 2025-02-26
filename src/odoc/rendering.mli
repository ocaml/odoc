open Odoc_utils
open Odoc_document

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
  sidebar:Fpath.t option ->
  'a ->
  Fpath.t ->
  (unit, [> msg ]) result

val generate_source_odoc :
  syntax:Renderer.syntax ->
  warnings_options:Odoc_model.Error.warnings_options ->
  renderer:'a Renderer.t ->
  output:Fs.directory ->
  source_file:Fpath.t ->
  extra_suffix:string option ->
  sidebar:Fpath.t option ->
  'a ->
  Fpath.t ->
  (unit, [> msg ]) result

val generate_asset_odoc :
  warnings_options:Odoc_model.Error.warnings_options ->
  renderer:'a Renderer.t ->
  output:Fs.directory ->
  asset_file:Fs.file ->
  extra_suffix:string option ->
  'a ->
  Fs.file ->
  (unit, [> msg ]) result

val targets_odoc :
  resolver:Resolver.t ->
  warnings_options:Odoc_model.Error.warnings_options ->
  syntax:Renderer.syntax ->
  renderer:'a Renderer.t ->
  output:Fs.directory ->
  extra:'a ->
  Fpath.t ->
  (unit, [> msg ]) result

val targets_source_odoc :
  syntax:Renderer.syntax ->
  warnings_options:Odoc_model.Error.warnings_options ->
  renderer:'a Renderer.t ->
  output:Fs.directory ->
  extra:'a ->
  source_file:Fpath.t ->
  Fs.file ->
  (unit, [> msg ]) result
