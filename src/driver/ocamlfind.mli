(** What findlib knows about the libraries installed in the switch.

    A library here is a findlib package, for example [fmt] or [tyxml.functor].
    Findlib is initialised from the switch's [findlib.conf], or by hand if there
    is none, so these answers hold for the switch the driver documents. *)

val add_path : Fpath.t -> unit
(** Search a directory before the configured ones. Voodoo mode adds the [lib] of
    the package it documents. Must be called before any other query. *)

val libraries_of_meta : Fpath.t -> string list
(** The libraries a [META] file defines, as findlib reads it: the library named
    after the file, [<name>/META] or [META.<name>], and its sub-libraries.
    Private libraries are left out. *)

val all : unit -> string list
(** Every library findlib knows. *)

val get_dir : string -> (Fpath.t, [> `Msg of string ]) result
(** The directory a library's objects are installed in. An error for a library
    that is not installed. *)

val archives : string -> string list
(** The archive files of a library, for example [["fmt.cma"; "fmt.cmxa"]]. A
    library with no archive is either virtual or an alias for the libraries it
    requires. *)

val direct_deps : string -> (Util.StringSet.t, [> `Msg of string ]) result
(** The libraries a library requires directly: its [META] [requires] field under
    the [ppx_driver] predicate, plus [stdlib]. The field is read as written, so
    a requirement on a library that is not installed is kept. An error for a
    library that is not installed. *)
