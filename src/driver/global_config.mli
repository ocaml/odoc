type deps = { packages : string list; libraries : string list }

type t = { deps : deps }

val empty : t

val parse : string -> t

val load : Fpath.t -> t
