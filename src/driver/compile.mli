type compiled

val init_stats : Packages.set -> unit

val compile :
  ?partial:Fpath.t ->
  output_dir:Fpath.t ->
  ?linked_dir:Fpath.t ->
  (* Packages.set *) Odoc_unit.t list (* Util.StringMap.t *) ->
  compiled list
(** Use [partial] to reuse the output of a previous call to [compile]. Useful in
    the voodoo context.

    [output_dir] is the directory for [odoc] file, [linked_dir] is the one for
    [odocl] files (defaulting to [output_dir] when absent). *)

type linked

val link : compiled list -> linked list

(* val index : odocl_dir:Fpath.t -> Packages.set -> unit *)

(* val sherlodoc : html_dir:Fpath.t -> odocl_dir:Fpath.t -> Packages.set -> unit *)

val html_generate : Fpath.t (* -> odocl_dir:Fpath.t *) -> linked list -> unit
