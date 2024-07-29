type pkg_args = {
  pages : (string * Fpath.t) list;
  libs : (string * Fpath.t) list;
}

type 'a unit = {
  parent_id : Odoc.id;
  odoc_dir : Fpath.t;
  input_file : Fpath.t;
  output_dir : Fpath.t;
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  pkg_args : pkg_args;
  pkgname : string;
  include_dirs : Fpath.t list;
  kind : 'a;
}

type intf_extra = { hidden : bool; hash : string; deps : intf unit list }
and intf = [ `Intf of intf_extra ]

type impl = [ `Impl ]

type mld = [ `Mld ]

type t = [ impl | intf | mld ] unit

val of_packages :
  output_dir:Fpath.t -> linked_dir:Fpath.t option -> Packages.t list -> t list
