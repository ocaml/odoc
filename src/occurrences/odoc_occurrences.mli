open Odoc_model.Lang

module Table = Table

val of_impl : include_hidden:bool -> Implementation.t -> Table.t -> unit
(** Add all occurrences from implementation of a compilation unit into a table *)

val aggregate : tbl:Table.t -> data:Table.t -> unit
(** Aggregate [data] into [tbl] *)
