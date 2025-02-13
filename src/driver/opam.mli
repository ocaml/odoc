type package = { name : string; version : string }

type doc_file = {
  kind : [ `Mld | `Asset | `Other ];
  file : Fpath.t;
  rel_path : Fpath.t;
}

type installed_files = {
  libs : Fpath.set;
  docs : doc_file list;
  odoc_config : Fpath.t option;
}

type package_of_fpath = package Fpath.map

(* Here we use an associative list *)
type fpaths_of_package = (package * installed_files) list
val all_opam_packages : unit -> package list

val classify_docs : Fpath.t -> string option -> Fpath.t list -> doc_file list

val check : string list -> (unit, Util.StringSet.t) Result.t
val deps : string list -> package list
val pkg_to_dir_map : unit -> fpaths_of_package * package_of_fpath
val pp : Format.formatter -> package -> unit
val prefix : unit -> string
