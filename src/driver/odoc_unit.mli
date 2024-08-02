type pkg_args = {
  pages : (string * Fpath.t) list;
  libs : (string * Fpath.t) list;
}

type index = {
  pkg_args : pkg_args;
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
  pkg_args : pkg_args;
  pkgname : string;
  include_dirs : Fpath.t list;
  index : index option;
  kind : 'a;
}

type intf_extra = { hidden : bool; hash : string; deps : intf unit list }
and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]

type t = [ impl | intf | mld ] unit

val of_packages :
  output_dir:Fpath.t ->
  linked_dir:Fpath.t option ->
  index_dir:Fpath.t option ->
  Packages.t list ->
  t list
