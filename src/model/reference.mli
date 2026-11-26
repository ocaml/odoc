type path =
  [ `Root of string
  | `LibraryRoot of [ `Only | `First ] * string * string
    (** [`LibraryRoot (`Only, lib, name)]: the root [name] in library [lib]
        alone, written [/lib/name]. TEMPORARY: [`First] looks in [lib] first,
        then anywhere. *)
  | `Dot of path * string ]

val parse :
  Location_.span -> string -> Paths.Reference.t Error.with_errors_and_warnings

val parse_asset :
  Location_.span ->
  string ->
  Paths.Reference.Asset.t Error.with_errors_and_warnings

val own_library : string option ref
(** TEMPORARY, until dune writes [@canonical /lib/Name.X]. [Some lib] while
    [odoc compile] reads a unit of the library [lib]. A canonical tag that names
    no library is then read as [`LibraryRoot (`First, lib, _)]: its root is
    looked up in [lib] first, and anywhere if no directory given with [-L] for
    [lib] holds a unit of that name. *)

val read_path_longident :
  Location_.span -> string -> path Error.with_errors_and_warnings

val read_mod_longident :
  Location_.span ->
  string ->
  Paths.Reference.Module.t Error.with_errors_and_warnings
