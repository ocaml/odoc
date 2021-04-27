(* Link *)

open Odoc_model

val signature :
  Env.t -> Paths.Identifier.Signature.t -> Lang.Signature.t -> Lang.Signature.t
(** For testing purpose. May call [Lookup_failures.report]. *)

val link :
  filename:string ->
  Env.t ->
  Lang.Compilation_unit.t ->
  Lang.Compilation_unit.t Error.with_warnings

val resolve_page :
  filename:string -> Env.t -> Lang.Page.t -> Lang.Page.t Error.with_warnings
