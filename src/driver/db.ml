(* Db - a type to help determine which modules belong in which libraries *)

type t = {
  all_libs : Util.StringSet.t;
  all_lib_deps : Util.StringSet.t Util.StringMap.t;
  lib_dirs_and_archives : (string * Fpath.t * Util.StringSet.t) list;
  archives_by_dir : Util.StringSet.t Fpath.map;
  libname_of_archive : string Fpath.map;
  cmi_only_libs : (Fpath.t * string) list;
}
