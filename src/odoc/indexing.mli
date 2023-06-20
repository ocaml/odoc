open Or_error

val handle_file :
  Fpath.t ->
  unit:(Odoc_model.Lang.Compilation_unit.t -> 'a) ->
  page:(Odoc_model.Lang.Page.t -> 'a) ->
  ('a option, [> msg ]) result

val compile :
  resolver:'a ->
  parent:'b ->
  output:Fs.file ->
  warnings_options:Odoc_model.Error.warnings_options ->
  Fs.directory list ->
  (unit, [> msg ]) result
