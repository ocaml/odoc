open Or_error

val handle_file :
  Fpath.t ->
  unit:(Odoc_model.Lang.Compilation_unit.t -> 'a) ->
  page:(Odoc_model.Lang.Page.t -> 'a) ->
  ('a, [> msg ]) result
(** This function is exposed for custom indexers that uses [odoc] as a library
    to generate their search index *)

val compile :
  output:Fs.file ->
  warnings_options:Odoc_model.Error.warnings_options ->
  Fs.file list ->
  Fs.file list ->
  (unit, [> msg ]) result
