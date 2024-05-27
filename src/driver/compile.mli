type ty = Module of Packages.modulety | Mld of Packages.mld

type compiled = {
  m : ty;
  output_dir : Fpath.t;
  output_file : Fpath.t;
  include_dirs : Fpath.Set.t;
  impl : (Fpath.t * Fpath.t) option;
}

val init_stats : Packages.set -> unit

val compile : Fpath.t -> Packages.set -> compiled list

type linked = { output_file : Fpath.t; src : Fpath.t option }

val link : compiled list -> linked list

val html_generate : Fpath.t -> linked list -> unit
