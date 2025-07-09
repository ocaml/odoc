val find_universe_and_version :
  string -> (string * string, [> `Msg of string ]) result

type pkg

val find_pkg : string -> blessed:bool -> pkg option
(** [get_pkg name ~blessed] looks for a package named [name] in the prep
    directory *)

val of_voodoo : pkg -> Packages.t

val occurrence_file_of_pkg : pkg -> Fpath.t
(** [occurrences_file_of_pkg pkg odoc_dir] returns an appropriate filename for
    the occurrences file for [pkg]. *)

type extra_paths = {
  pkgs : Fpath.t Util.StringMap.t;
  libs : Fpath.t Util.StringMap.t;
  libs_of_pkg : string list Util.StringMap.t;
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
