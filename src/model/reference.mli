type path = [ `Root of string | `Dot of Paths.Path.Module.t * string ]

val parse :
  Location_.span -> string -> Paths.Reference.t Error.with_errors_and_warnings

val read_path_longident :
  Location_.span -> string -> path Error.with_errors_and_warnings

val read_mod_longident :
  Location_.span ->
  string ->
  Paths.Reference.Module.t Error.with_errors_and_warnings
