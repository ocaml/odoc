type compiled = Odoc_unit.any

val init_stats : Odoc_unit.any list -> unit

val compile :
  ?partial:Fpath.t -> partial_dir:Fpath.t -> Odoc_unit.any list -> compiled list
(** Use [partial] to reuse the output of a previous call to [compile]. Useful in
    the voodoo context.

    [output_dir] is the directory for [odoc] file, [linked_dir] is the one for
    [odocl] files (defaulting to [output_dir] when absent). *)

type linked

val link :
  warnings_tags:string list ->
  custom_layout:bool ->
  compiled list ->
  linked list

val html_generate :
  occurrence_file:Fpath.t ->
  remaps:(string * string) list ->
  generate_json:bool ->
  simplified_search_output:bool ->
  Fpath.t ->
  linked list ->
  unit
