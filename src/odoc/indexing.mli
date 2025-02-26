open Odoc_utils

val compile :
  [ `JSON | `Marshall ] ->
  output:Fs.file ->
  warnings_options:Odoc_model.Error.warnings_options ->
  occurrences:Fs.file option ->
  roots:Fs.Directory.t list ->
  inputs_in_file:Fs.file list ->
  simplified_json:bool ->
  wrap_json:bool ->
  odocls:Fs.file list ->
  (unit, [> msg ]) result
