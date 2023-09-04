open Or_error

val handle_file :
  Fpath.t ->
  unit:(Odoc_model.Lang.Compilation_unit.t -> 'a) ->
  page:(Odoc_model.Lang.Page.t -> 'a) ->
  ('a option, [> msg ]) result
(** This function is exposed for custom indexers that uses [odoc] as a library
    to generate their search index *)

val compile : output:Fs.file -> Fs.directory list -> (unit, [> msg ]) result
