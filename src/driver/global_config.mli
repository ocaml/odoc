(** The [odoc-config.sexp] file a package may install.

    It lists the other packages and libraries whose documentation the package's
    pages refer to, which the driver adds to the package's reference scope
    ({!Odoc_unit.type-scope}). The file is installed at
    [doc/<pkg>/odoc-config.sexp] and holds stanzas of the form
    [(packages p1 p2 ...)] and [(libraries l1 l2 ...)]. *)

type deps = { packages : string list; libraries : string list }
type t = { deps : deps }

val empty : t
(** No extra dependencies. *)

val parse : string -> t
(** Parse the contents of a config file. Raises on malformed input. *)

val load : Fpath.t -> t
(** Read and parse a config file. A missing or malformed file is reported and
    treated as {!empty}. *)
