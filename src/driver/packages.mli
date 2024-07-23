(** {1 OCaml compilation unit} *)

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
  m_pkg_dir : Fpath.t;
}

(** {1 Standalone pages units} *)

type mld = {
  mld_odoc_file : Fpath.t;
  mld_odocl_file : Fpath.t;
  mld_parent_id : id;
  mld_path : Fpath.t;
  mld_deps : Fpath.t list;
  mld_pkg_dir : Fpath.t;
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

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  mld_odoc_dir : Fpath.t;
      (** Relative to dir where all odoc files are, e.g. [_odoc/] by default *)
  pkg_dir : Fpath.t;
  other_docs : Fpath.Set.t;
}

val pp : Format.formatter -> t -> unit

type set = t Util.StringMap.t

val of_libs : Fpath.t option -> Util.StringSet.t -> set
(** Turns a set of libraries into a map from library name to package *)

val parent_of_pkg : Fpath.t -> Fpath.t

module Lib : sig
  val v :
    Fpath.t ->
    string Util.StringMap.t ->
    string ->
    Fpath.t ->
    Fpath.t option ->
    libty list
end
