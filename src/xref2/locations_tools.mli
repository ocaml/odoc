open Odoc_model

val lookup_loc : Env.t -> Lang.Locations.unresolved -> Lang.Locations.t option
(** Lookup the {!Lang.Locations} corresponding to an unresolved path. *)
