module Pkg_args : sig
  type t = {
    compile_dir : Fpath.t;
    link_dir : Fpath.t;
    pages : (string * Fpath.t) list;
    libs : (string * Fpath.t) list;
  }

  val compiled_pages : t -> (string * Fpath.t) list
  val compiled_libs : t -> (string * Fpath.t) list
  val linked_pages : t -> (string * Fpath.t) list
  val linked_libs : t -> (string * Fpath.t) list

  val combine : t -> t -> t

  val pp : t Fmt.t
end

type index = {
  pkg_args : Pkg_args.t;
  output_file : Fpath.t;
  json : bool;
  search_dir : Fpath.t;
}

type 'a unit = {
  parent_id : Odoc.Id.t;
  odoc_dir : Fpath.t;
  input_file : Fpath.t;
  output_dir : Fpath.t;
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  pkg_args : Pkg_args.t;
  pkgname : string;
  include_dirs : Fpath.Set.t;
  index : index option;
  kind : 'a;
}

type intf_extra = { hidden : bool; hash : string; deps : intf unit list }
and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]

type asset = [ `Asset ]

type t = [ impl | intf | mld | asset ] unit

val pp : t Fmt.t

val lib_dir : Packages.t -> Packages.libty -> Fpath.t
val doc_dir : Packages.t -> Fpath.t

val of_packages :
  output_dir:Fpath.t ->
  linked_dir:Fpath.t option ->
  index_dir:Fpath.t option ->
  extra_libs_paths:Fpath.t Util.StringMap.t ->
  Packages.t list ->
  t list
