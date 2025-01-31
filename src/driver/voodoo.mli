val find_universe_and_version :
  string -> (string * string, [> `Msg of string ]) result

val of_voodoo : string -> blessed:bool -> Packages.t list

type extra_paths = {
  pkgs : Fpath.t Util.StringMap.t;
  libs : Fpath.t Util.StringMap.t;
}

val empty_extra_paths : extra_paths
(** When [odoc_driver] is not running in voodoo mode, this value can be passed
    to {!Odoc_units_of.packages} *)

val extra_paths : Fpath.t -> extra_paths
(** [extra_paths odoc_dir] returns the paths to packages and libraries that have
    previously been compiled by odoc_driver running in voodoo mode. In order to
    find these, the previous invocation of odoc_driver will need to have written
    marker files by calling {!write_lib_markers} *)

val write_lib_markers : Fpath.t -> Packages.t list -> unit
(** [write_lib_markers odoc_dir pkgs] writes marker files to show the locations
    of the compilation units associated with packages and libraries in [pkgs].
*)
