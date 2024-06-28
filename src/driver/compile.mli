type compiled

val init_stats : Packages.set -> unit

val compile : Fpath.t option -> output_dir:Fpath.t -> ?linked_dir:Fpath.t -> Packages.set -> compiled list

type linked

val link : compiled list -> linked list

val index : odocl_dir:Fpath.t -> Packages.set -> unit

val sherlodoc : html_dir:Fpath.t -> odocl_dir:Fpath.t -> Packages.set -> unit

(* val compile_sidebars : *)
(*   odoc_dir:Fpath.t -> *)
(*   output_dir:Fpath.t -> *)
(*   Packages.set -> *)
(*   Fpath.t Util.StringMap.t *)

val html_generate : Fpath.t -> odocl_dir:Fpath.t -> linked list -> unit
