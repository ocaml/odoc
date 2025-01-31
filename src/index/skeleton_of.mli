open Odoc_model

(** Page hierarchies represent a hierarchy of pages. *)

val lang :
  pages:Lang.Page.t list ->
  modules:Lang.Compilation_unit.t list ->
  implementations:Lang.Implementation.t list ->
  Skeleton.t
(** Uses the convention that the [index] children passes its payload to the
    container directory to output a payload *)
