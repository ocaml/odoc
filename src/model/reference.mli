type path =
  [ `Root of string
  | `LibraryRoot of [ `Only ] * string * string
    (** [`LibraryRoot (`Only, lib, name)]: the root [name] in library [lib]
        alone, written [/lib/name]. *)
  | `Dot of path * string ]

val parse :
  Location_.span -> string -> Paths.Reference.t Error.with_errors_and_warnings

val parse_asset :
  Location_.span ->
  string ->
  Paths.Reference.Asset.t Error.with_errors_and_warnings

val read_path_longident :
  Location_.span -> string -> path Error.with_errors_and_warnings

val read_mod_longident :
  Location_.span ->
  string ->
  Paths.Reference.Module.t Error.with_errors_and_warnings
