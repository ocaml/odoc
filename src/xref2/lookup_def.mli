(** Find the location of the definition for a identifier. *)

open Odoc_model.Paths

val lookup_def :
  Env.t -> [< Identifier.t_pv ] Identifier.id -> Shape.t option
