(** {1 OCaml compilation unit} *)

type pkgname = { p_name : string; p_dir : Fpath.t }
(** A package's name and path. *)

(** {2 Interface part} *)

type dep = string * Digest.t

type id = Odoc.id

type intf = {
  mif_odoc_file : Fpath.t;
  mif_odocl_file : Fpath.t;
  mif_parent_id : id;
  mif_hash : string;
  mif_path : Fpath.t;
  mif_deps : dep list;
}

val pp_intf : Format.formatter -> intf -> unit

(** {2 Implementation part} *)

type src_info = { src_path : Fpath.t; src_id : id }

type impl = {
  mip_odoc_file : Fpath.t;
  mip_odocl_file : Fpath.t;
  mip_parent_id : id;
  mip_path : Fpath.t;
  mip_src_info : src_info option;
}

val pp_impl : Format.formatter -> impl -> unit

(** {2 OCaml Compilation unit} *)

type modulety = {
  m_name : string;
  m_intf : intf;
  m_impl : impl option;
  m_hidden : bool;
  m_pkgname : pkgname;
}

(** {1 Standalone pages units} *)

type mld = {
  mld_odoc_file : Fpath.t;
  mld_odocl_file : Fpath.t;
  mld_parent_id : id;
  mld_path : Fpath.t;
  mld_deps : Fpath.t list;
  mld_pkgname : pkgname;
}

val pp_mld : Format.formatter -> mld -> unit

(** {1 Packages} *)

(** Compilation units are associated to libraries, while documentation are
    associated to package *)

type libty = {
  lib_name : string;
  odoc_dir : Fpath.t;
      (** Relative to dir where all odoc files are, e.g. [_odoc/] by default *)
  archive_name : string;
  modules : modulety list;
}

val parent_of_pages : pkgname -> Fpath.t
(** Given a [pkgname], returns a [mld_odoc_dir]. *)

module Lib : sig
  val v :
    pkgname:pkgname ->
    libname_of_archive:string Util.StringMap.t ->
    dir:Fpath.t ->
    cmtidir:Fpath.t option ->
    libty list
end

type t = {
  pkgname : pkgname;
  version : string;
  libraries : libty list;
  mlds : mld list;
  mld_odoc_dir : Fpath.t;
      (** Relative to dir where all odoc files are, e.g. [_odoc/] by default *)
  other_docs : Fpath.Set.t;
}

val pp : Format.formatter -> t -> unit

type set = t Util.StringMap.t

val of_libs : packages_dir:Fpath.t option -> Util.StringSet.t -> set
(** Turns a set of libraries into a map from package name to package *)
