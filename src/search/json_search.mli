(** This module generates json intended to be consumed by search engines. *)

val unit : Format.formatter -> Odoc_model.Lang.Compilation_unit.t -> unit
val page : Format.formatter -> Odoc_model.Lang.Page.t -> unit
