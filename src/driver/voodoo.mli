val find_universe_and_version :
  string -> (string * string, [> `Msg of string ]) result

val of_voodoo : string -> blessed:bool -> Packages.set
