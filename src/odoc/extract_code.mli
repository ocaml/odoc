val extract :
  dst:string option ->
  input:string ->
  names:string list ->
  line_directives:bool ->
  warnings_options:Odoc_model.Error.warnings_options ->
  (unit, [> `Msg of string ]) result
