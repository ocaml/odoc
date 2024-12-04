(** This module generates json intended to be consumed by search engines. *)

val of_entry :
  simplified:bool ->
  ?occurrences:Odoc_occurrences.Table.t ->
  Odoc_index.Entry.t ->
  Odoc_utils.Json.json
