(* Compile *)

open Odoc_model

val signature :
  Env.t -> Paths.Identifier.Signature.t -> Lang.Signature.t -> Lang.Signature.t
(** For testing purpose. May call [Lookup_failures.report]. *)

val compile :
  filename:string ->
  Env.t ->
  Lang.Compilation_unit.t ->
  Lang.Compilation_unit.t Error.with_warnings
(** [filename] is used for generating warnings. *)

val resolve_page : 'a -> 'b -> 'b
