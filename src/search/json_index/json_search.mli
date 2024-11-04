(** This module generates json intended to be consumed by search engines. *)

val unit :
  ?occurrences:Odoc_occurrences.Table.t ->
  Format.formatter ->
  Odoc_model.Lang.Compilation_unit.t ->
  unit
val page : Format.formatter -> Odoc_model.Lang.Page.t -> unit
val index :
  ?occurrences:Odoc_occurrences.Table.t ->
  Format.formatter ->
  Odoc_index.Entry.t Odoc_model.Paths.Identifier.Hashtbl.Any.t ->
  unit
