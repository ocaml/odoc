open Or_error

val compile :
  resolver:Resolver.t ->
  parent:string ->
  output:Fs.File.t ->
  warnings_options:Odoc_model.Error.warnings_options ->
  Fs.File.t ->
  (unit, [> msg ]) result
(** Produces a compiled page ([.odoc]) from a file containing a list of paths.
    The [resolver] is only used to lookup the parent page. *)
