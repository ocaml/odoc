type compiled

val init_stats : Packages.set -> unit

val compile : Fpath.t -> Packages.set -> compiled list

type linked

val link : compiled list -> linked list

val compile_sidebars :
  Fpath.t -> Fpath.t -> Packages.set -> Fpath.t Util.StringMap.t

val html_generate : Fpath.t -> Fpath.t Util.StringMap.t -> linked list -> unit
