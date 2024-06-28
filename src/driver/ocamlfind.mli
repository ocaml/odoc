val get_dir : string -> (Fpath.t, [> `Msg of string ]) result
(** [get_dir libname] returns the path where [libname] is installed *)

val all : unit -> string list
(** [all ()] returns a list of all packages *)

val archives : string -> string list
(** Returns the list of archive file names of a given library *)

val sub_libraries : string -> Util.StringSet.t
(** Returns the list of sublibraries of a given library *)

val deps : string list -> (string list, [> `Msg of string ]) result
(** Returns the list of transitive package dependencies of given libraries *)
