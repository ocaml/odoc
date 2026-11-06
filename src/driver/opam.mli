(** What opam knows about the switch: its packages and the files each one
    installed. *)

type package = { name : string; version : string }

val pp : Format.formatter -> package -> unit

val prefix : unit -> string
(** The switch prefix, the directory holding [lib], [doc] and so on. *)

val install_roots : unit -> Fpath.t list
(** The directories below which installed files live: the switch prefix and,
    when the driver runs under [dune exec], dune's install directory. *)

val all_opam_packages : unit -> package list
(** Every package installed in the switch. *)

val deps : string list -> package list
(** The given packages and, transitively, the packages they depend on. *)

val doc_deps : package -> string list
(** The packages a package's documentation needs beyond its dependencies: the
    dependencies its opam file marks [{with-doc & post}], and the packages in
    its [x-extra-doc-deps] field. ocaml-docs-ci installs them after the package
    is compiled and before it is linked. Never the package itself. *)

val check : string list -> (unit, Util.StringSet.t) Result.t
(** Check that the given packages are installed. The error holds the names of
    those that are not. *)

type doc_file = {
  kind : [ `Mld | `Asset | `Other ];
      (** An [.mld] page, an asset shipped with the pages, or another file such
          as a README. *)
  file : Fpath.t;  (** Its full path. *)
  rel_path : Fpath.t;  (** Its path below the pages directory. *)
}
(** A file a package installed below [doc/<pkg>]. *)

val classify_docs : Fpath.t -> string option -> Fpath.t list -> doc_file list
(** [classify_docs prefix pkg files] picks out the documentation among the
    [files] a package installed, given relative to [prefix]. With [Some pkg],
    only files below [doc/<pkg>] are considered. *)

type installed_files = {
  libs : Fpath.set;  (** The directories below [lib] holding [.cmi] files. *)
  docs : doc_file list;
  odoc_config : Fpath.t option;  (** Its [odoc-config.sexp], if any. *)
}
(** The files a package installed that matter to the driver. *)

type package_of_fpath = package Fpath.map
type fpaths_of_package = (package * installed_files) list

val pkg_to_dir_map : unit -> fpaths_of_package * package_of_fpath
(** For every installed package, the files it installed; and, the other way
    round, the package that installed each library directory. Read from opam's
    record of installed files. *)
