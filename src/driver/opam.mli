type package = { name : string; version : string }

type installed_files = {
  libs : Fpath.set;
  odoc_pages : Fpath.set;
  other_docs : Fpath.set;
}

type package_of_fpath = package Fpath.map

(* Here we use an associative list *)
type fpaths_of_package = (package * installed_files) list

val pkg_to_dir_map : unit -> fpaths_of_package * package_of_fpath
val pp : Format.formatter -> package -> unit
val prefix : unit -> string
