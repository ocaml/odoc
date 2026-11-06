(** The packages to document, as found in the switch.

    A package has libraries and documentation. A library has modules; each
    module has an interface and may have an implementation. Everything the later
    steps need about a package is gathered here, once, before anything is
    compiled. *)

(** {1 Modules} *)

type dep = string * Digest.t
(** A module a unit depends on: its name and the digest of its interface, as
    reported by [odoc compile-deps]. *)

type intf = {
  mif_hash : string;  (** The digest of the interface. *)
  mif_path : Fpath.t;
  mif_deps : dep list;  (** The modules the interface refers to. *)
}
(** The interface of a module: a [.cmti] file, or the [.cmt] file when there is
    no [.mli]. *)

type src_info = { src_path : Fpath.t }

type impl = {
  mip_path : Fpath.t;
  mip_src_info : src_info option;  (** Its source file, when found. *)
}
(** The implementation of a module: its [.cmt] file. *)

type modulety = {
  m_name : string;
  m_intf : intf;
  m_impl : impl option;
  m_hidden : bool;
      (** A module whose name contains [__], not shown to readers. *)
}

(** {1 Documentation} *)

type mld = { mld_path : Fpath.t; mld_rel_path : Fpath.t }
(** An [.mld] page, with its path below the package's pages directory. *)

type md = { md_path : Fpath.t; md_rel_path : Fpath.t }
(** A Markdown file, such as a README, likewise. *)

type asset = { asset_path : Fpath.t; asset_rel_path : Fpath.t }
(** A file shipped with the pages, such as an image, likewise. *)

val mk_mlds : Opam.doc_file list -> mld list * asset list * md list
(** Sort a package's documentation files into pages, assets and other files. *)

(** {1 Libraries} *)

type libty = {
  lib_name : string;  (** The findlib name. *)
  dir : Fpath.t;  (** Where its objects are. *)
  rel_dir : Fpath.t;
      (** [dir] relative to its install root. The library's [.odoc] files are
          written at this path below the odoc directory, so that the odoc
          directory mirrors the switch. See {!Odoc_unit.lib_obj_dir}. *)
  archive_name : string option;
      (** The archive without its extension. [None] for a virtual library. *)
  lib_deps : Util.StringSet.t;  (** The libraries it requires directly. *)
  modules : modulety list;
}

module Lib : sig
  val rel_dir : roots:Fpath.t list -> Fpath.t -> Fpath.t
  (** [rel_dir ~roots dir] is [dir] relative to the first of [roots] that
      contains it. See {!Opam.install_roots}. *)

  val v :
    roots:Fpath.t list ->
    installed:Installed.t ->
    pkg_name:string ->
    dir:Fpath.t ->
    libty list
  (** The libraries whose objects are in [dir]. Each archive found there gives a
      library, named by {!Installed.lib_of_archive}, or after the archive if no
      library declares it. A directory with no archive is a library with no
      archive if {!Installed.no_archive_lib} names it. Every module is analysed
      with [odoc compile-deps]. *)

  val pp : Format.formatter -> libty -> unit
end

(** {1 Packages} *)

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  assets : asset list;
  other_docs : md list;
  pkg_dir : Fpath.t;
      (** The root of the package's pages in the HTML output, and the parent id
          of its top-level page. *)
  doc_dir : Fpath.t;
      (** The parent id below which its pages live. Equal to [pkg_dir] for an
          opam package; [pkg_dir/doc] in voodoo mode. *)
  config : Global_config.t;  (** Its [odoc-config.sexp]. *)
  doc_deps : string list;
      (** The packages its documentation needs beyond its dependencies
          ({!Opam.doc_deps}), compiled before it is linked. None in voodoo mode,
          where ocaml-docs-ci orders the jobs. *)
}

val remaps : t -> (string * string) list
(** The prefixes of links into this package, each with the link to its
    documentation on ocaml.org. A driver that renders only some of the packages
    gives these to [odoc html-generate] for the rest, so that links into them
    lead somewhere. *)

val pp : Format.formatter -> t -> unit

val of_packages :
  installed:Installed.t -> packages_dir:Fpath.t option -> string list -> t list
(** [of_packages ~packages_dir names] finds the named packages and everything
    they depend on in the switch, and analyses all their modules. With no names,
    every installed package. Which of them the run was asked for is not recorded
    here: it is a fact about the run, and {!Odoc_units_of.packages} takes it.
    [packages_dir] is a prefix for every package's [pkg_dir]. *)

val remap_virtual : t list -> t list
(** Point the modules of a virtual library's implementations at the virtual
    library's interface.

    An implementation ships its modules as [.cmt] files only. The [.mli], and
    the documentation written in it, belong to the virtual library, which ships
    the [.cmti]. Both have the same interface digest. For every module whose
    interface is a [.cmt] and whose digest is shared with a [.cmti] elsewhere in
    the given packages, use that [.cmti] instead. *)
