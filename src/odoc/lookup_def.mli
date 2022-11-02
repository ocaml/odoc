(** Find the location of the definition of an identifier. *)

open Odoc_model.Paths

val lookup_def :
  Odoc_loader.typing_env -> Identifier.t -> Odoc_model.Location_.span option
