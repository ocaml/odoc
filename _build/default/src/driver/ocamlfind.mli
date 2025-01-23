val get_dir : string -> (Fpath.t, [> `Msg of string ]) result
(** [get_dir libname] returns the path where [libname] is installed *)

val all : unit -> string list
(** [all ()] returns a list of all packages *)

val archives : string -> string list
(** Returns the list of archive file names of a given library *)

val sub_libraries : string -> Util.StringSet.t
(** Returns the list of sublibraries of a given library *)

val deps : string list -> (Util.StringSet.t, [> `Msg of string ]) result
(** Returns the list of transitive package dependencies of given libraries *)

module Db : sig
  type t = {
    all_libs : Util.StringSet.t;
    all_lib_deps : Util.StringSet.t Util.StringMap.t;
    lib_dirs_and_archives : (string * Fpath.t * Util.StringSet.t) list;
    archives_by_dir : Util.StringSet.t Fpath.map;
    libname_of_archive : string Fpath.map;
    cmi_only_libs : (Fpath.t * string) list;
  }

  val create : Util.StringSet.t -> t
end
