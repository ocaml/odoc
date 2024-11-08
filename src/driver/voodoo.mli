val find_universe_and_version :
  string -> (string * string, [> `Msg of string ]) result

val of_voodoo : string -> blessed:bool -> Packages.set

val extra_paths : Fpath.t -> Fpath.t Util.StringMap.t * Fpath.t Util.StringMap.t

val write_lib_markers : Fpath.t -> Packages.t Util.StringMap.t -> unit
