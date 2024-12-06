type library = {
  name : string;
  archive_name : string option;
  dir : string option;
  deps : string list;
}

type t = { meta_dir : Fpath.t; libraries : library list }

val process_meta_file : Fpath.t -> t
(** From a path to a [Meta] file, returns the list of libraries defined in this
    file. *)

val libname_of_archive : t -> string Fpath.map
(** [libname_of_archive meta_dir libraries] computes a map from the
    fully-qualified archive path to the name of the library. [meta_path] is the
    path of the directory where the META file is found, and [libraries] are the
    libraries defined in that META file. *)

val directories : t -> Fpath.set
(** [directories meta_dir libraries] computes a set of directories containing
    the libraries in [libraries] defined in the META file found in [meta_path].
*)
