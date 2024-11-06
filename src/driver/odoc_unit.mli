module Pkg_args : sig
  type t = {
    odoc_dir : Fpath.t;
    odocl_dir : Fpath.t;
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
  input_file : Fpath.t;
  output_dir : Fpath.t;
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  pkg_args : Pkg_args.t;
  pkgname : string option;
  include_dirs : Fpath.Set.t;
  index : index option;
  enable_warnings : bool;
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

type dirs = {
  odoc_dir : Fpath.t;
  odocl_dir : Fpath.t;
  index_dir : Fpath.t;
  mld_dir : Fpath.t;
}
