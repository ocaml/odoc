(** Find the location of the definition of an identifier. *)

open Odoc_model.Paths

module Make (Loader : sig
  val read_typing_env : unit_name:string -> Odoc_loader.typing_env option
end) : sig
  val lookup_def : Ocaml_env.t -> Identifier.t -> Location.t option
end
