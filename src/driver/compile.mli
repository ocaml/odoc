type compiled

val init_stats : Packages.set -> unit

val compile :
  ?partial:Fpath.t ->
  partial_dir:Fpath.t ->
  ?linked_dir:Fpath.t ->
  Odoc_unit.t list ->
  compiled list
(** Use [partial] to reuse the output of a previous call to [compile]. Useful in
    the voodoo context.

    [output_dir] is the directory for [odoc] file, [linked_dir] is the one for
    [odocl] files (defaulting to [output_dir] when absent). *)

type linked

val link : compiled list -> linked list

val html_generate : Fpath.t -> linked list -> unit
