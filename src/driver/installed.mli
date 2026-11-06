(** What is installed: the libraries findlib knows, and opam's record of which
    package installed which files.

    It is built once per run, and every other part of the driver asks it rather
    than findlib or opam. A library is a findlib package, for example [fmt] or
    [tyxml.functor]. *)

type lib = {
  name : string;
  dir : Fpath.t;  (** The directory its objects are installed in. *)
  archives : string list;  (** Its archives, without their extensions. *)
  requires : Util.StringSet.t;
      (** The libraries it requires directly: its [META] [requires] under the
          [ppx_driver] predicate, read as written, and [stdlib]. *)
}

type t

val of_switch : unit -> t
(** Every library findlib knows in the switch, with what findlib says about it.
    Which package installed each library comes from opam: from the record of the
    files each package installed ([.opam-switch/install/<pkg>.changes] below the
    switch), and under [dune exec] from dune's install directory too. That
    record is read the first time it is needed, by {!package},
    {!libs_of_package} or {!package_files}. *)

val of_prep : pkg_path:Fpath.t -> pkg_name:string -> libs:string list -> t
(** The libraries [libs] of the package voodoo-prep laid out in [pkg_path],
    which are [pkg_name]'s. A directory findlib places outside [pkg_path], as it
    does for [directory = "+"], is taken to be the package's copy of it. Any
    other library is looked up in findlib when asked for, and the package that
    installed it in opam's record, as for {!of_switch}. *)

val libs : t -> lib list
(** The libraries it was built from: every library of the switch, or the
    prepared package's. *)

val find : t -> string -> lib option
(** A library, or [None] if it is not installed. *)

val requires : t -> string -> Util.StringSet.t
(** {!lib.requires}, or [stdlib] alone for a library that is not installed. *)

val package : t -> string -> string option
(** The package that installed a library. *)

val libs_of_package : t -> string -> string list
(** The libraries a package installed. *)

val lib_of_archive : t -> Fpath.t -> string option
(** The library an archive, given by its full path without extension, belongs
    to. When two claim it, the one with the bytecode archive, else the name that
    sorts first. *)

val no_archive_lib : t -> Fpath.t -> string option
(** A library with no archive whose objects are in a directory: a virtual
    library, or one with no modules. *)

val package_files : t -> Opam.fpaths_of_package
(** The files each opam package installed. *)
