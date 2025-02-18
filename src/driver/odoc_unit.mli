module Pkg_args : sig
  type t

  val compiled_pages : t -> (string * Fpath.t) list
  val compiled_libs : t -> (string * Fpath.t) list
  val includes : t -> Fpath.t list
  val linked_pages : t -> (string * Fpath.t) list
  val linked_libs : t -> (string * Fpath.t) list

  val v :
    odoc_dir:Fpath.t ->
    odocl_dir:Fpath.t ->
    includes:Fpath.t list ->
    pages:(string * Fpath.t) list ->
    libs:(string * Fpath.t) list ->
    t

  val combine : t -> t -> t

  val pp : t Fmt.t
end

type sidebar = { output_file : Fpath.t; json : bool; pkg_dir : Fpath.t }
type index = {
  roots : Fpath.t list;
  output_file : Fpath.t;
  json : bool;
  search_dir : Fpath.t;
  sidebar : sidebar option;
}

type 'a t = {
  parent_id : Odoc.Id.t;
  input_file : Fpath.t;
  input_copy : Fpath.t option;
      (* Used to stash cmtis from virtual libraries into the odoc dir for voodoo mode *)
  output_dir : Fpath.t;
  odoc_file : Fpath.t;
  odocl_file : Fpath.t;
  pkg_args : Pkg_args.t;
  pkgname : string option;
  index : index option;
  enable_warnings : bool;
  to_output : bool;
  kind : 'a;
}

type intf_extra = {
  hidden : bool;
  hash : string;
  deps : (string * Digest.t) list;
}
and intf = [ `Intf of intf_extra ]

type impl_extra = { src_id : Odoc.Id.t; src_path : Fpath.t }
type impl = [ `Impl of impl_extra ]

type mld = [ `Mld ]
type md = [ `Md ]
type asset = [ `Asset ]

type any = [ impl | intf | mld | asset | md ] t

val pp : any Fmt.t

val pkg_dir : Packages.t -> Fpath.t
val lib_dir : Packages.t -> Packages.libty -> Fpath.t
val doc_dir : Packages.t -> Fpath.t
val src_dir : Packages.t -> Fpath.t
val src_lib_dir : Packages.t -> Packages.libty -> Fpath.t

type dirs = {
  odoc_dir : Fpath.t;
  odocl_dir : Fpath.t;
  index_dir : Fpath.t;
  mld_dir : Fpath.t;
}

val fix_virtual :
  precompiled_units:intf t list Util.StringMap.t ->
  units:intf t list Util.StringMap.t ->
  intf t list Util.StringMap.t
(** [fix_virtual ~precompiled_units ~units] replaces the input file in units
    representing implementations of virtual libraries. Implementation units have
    a [cmt] but no [cmti], even though the interface is actually constrained by
    a [mli]. The [cmi] file for the implementation is actually taken from the
    virtual library, so this function replaces the [cmt] used for the interface
    rendering of the implemenetation library units with the [cmti] taken from
    the virtual library. [units] should contain all the units that might be
    changed by this function. [precompiled_units] should be empty if this is
    being called before any compilation has taken place - ie. in monorepo or
    opam mode. In voodoo mode if the virtual library is in a different package
    it will have already been compiled. and thus should not be changed. The
    types of the inputs and outputs are hashtbls of units, where the hashtable
    key is the interface hash. *)
