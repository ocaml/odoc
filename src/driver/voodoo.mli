val find_universe_and_version :
  string -> (string * string, [> `Msg of string ]) result

val of_voodoo : string -> blessed:bool -> Packages.set

val extra_libs_paths : Fpath.t -> Fpath.t Util.StringMap.t
