(** [extract ~dst ~input ~names ~line_directives ~warnings_options] extracts source code from document blocks.
    @param dst File path to write to *)
val extract :
  dst:string option ->
  input:string ->
  names:string list ->
  line_directives:bool ->
  warnings_options:Odoc_model.Error.warnings_options ->
  (unit, [> `Msg of string ]) result
