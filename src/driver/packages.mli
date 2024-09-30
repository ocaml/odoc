(** {1 OCaml compilation unit} *)

(** {2 Interface part} *)

type dep = string * Digest.t

type intf = { mif_hash : string; mif_path : Fpath.t; mif_deps : dep list }

val pp_intf : Format.formatter -> intf -> unit

(** {2 Implementation part} *)

type src_info = { src_path : Fpath.t }

type impl = {
  mip_path : Fpath.t;
  mip_src_info : src_info option;
  mip_deps : dep list;
}

val pp_impl : Format.formatter -> impl -> unit

(** {2 OCaml Compilation unit} *)

type modulety = {
  m_name : string;
  m_intf : intf;
  m_impl : impl option;
  m_hidden : bool;
}

(** {1 Standalone pages units} *)

type mld = { mld_path : Fpath.t; mld_rel_path : Fpath.t }

val pp_mld : Format.formatter -> mld -> unit

(** {1 Asset units} *)

type asset = { asset_path : Fpath.t; asset_rel_path : Fpath.t }

val pp_asset : Format.formatter -> asset -> unit

(** {1 Packages} *)

(** Compilation units are associated to libraries, while documentation are
    associated to package *)

type libty = {
  lib_name : string;
  archive_name : string;
  modules : modulety list;
}

val parent_of_pages : Fpath.t -> Fpath.t
(** Given a [pkg_dir], returns a [mld_odoc_dir]. *)

module Lib : sig
  val v :
    libname_of_archive:string Util.StringMap.t ->
    pkg_name:string ->
    dir:Fpath.t ->
    cmtidir:Fpath.t option ->
    libty list
end

type t = {
  name : string;
  version : string;
  libraries : libty list;
  mlds : mld list;
  assets : asset list;
  other_docs : Fpath.Set.t;
  pkg_dir : Fpath.t;
  config : Global_config.t;
}

val pp : Format.formatter -> t -> unit

type set = t Util.StringMap.t

val of_libs : packages_dir:Fpath.t option -> Util.StringSet.t -> set
(** Turns a set of libraries into a map from package name to package *)
