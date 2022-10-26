(** Find the location of the definition of an identifier. *)

open Odoc_model
open Odoc_model.Paths

module Make (Loader : sig
  val lookup_root_module : string -> Lang.Compilation_unit.t option
end) : sig
  val lookup_def : Ocaml_env.t -> Identifier.t -> Location.t option
end
