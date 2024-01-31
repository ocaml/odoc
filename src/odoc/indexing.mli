open Or_error

val handle_file :
  Fpath.t ->
  unit:(Odoc_model.Lang.Compilation_unit.t -> 'a) ->
  page:(Odoc_model.Lang.Page.t -> 'a) ->
  occ:(Odoc_search.Entry.t Odoc_model.Paths.Identifier.Hashtbl.Any.t -> 'a) ->
  ('a, [> msg ]) result
(** This function is exposed for custom indexers that uses [odoc] as a library
    to generate their search index *)

val compile :
  [ `JSON | `Marshall ] ->
  output:Fs.file ->
  warnings_options:Odoc_model.Error.warnings_options ->
  occurrences:Fs.file option ->
  lib_roots:(string * Fs.directory) list ->
  page_roots:(string * Fs.directory) list ->
  inputs_in_file:Fs.file list ->
  odocls:Fs.file list ->
  (unit, [> msg ]) result
