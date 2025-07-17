open Odoc_model.Lang

module Table = Table

val of_impl : include_hidden:bool -> Implementation.t -> Table.t -> Table.Deftbl.t -> unit
(** Add all occurrences from implementation of a compilation unit into a table
*)

val aggregate : tbl:Table.t -> data:Table.t -> unit
(** Aggregate [data] into [tbl] *)

type hg_revision = string

val unspecified_hg_revision : hg_revision

type t = {table : Table.t; revision: hg_revision; max_occurrences: int}

val from_file : Fpath.t -> t

val to_file : t -> Fpath.t -> unit

val from_occtbl : Table.t -> hg_revision option -> t
