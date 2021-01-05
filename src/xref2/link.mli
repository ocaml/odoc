(* Link *)

val signature :
  Env.t ->
  Odoc_model.Paths.Identifier.Signature.t ->
  Odoc_model.Lang.Signature.t ->
  Odoc_model.Lang.Signature.t

val link :
  Env.resolver ->
  Odoc_model.Lang.Compilation_unit.t ->
  Odoc_model.Lang.Compilation_unit.t Lookup_failures.with_failures

val resolve_page :
  Env.resolver ->
  Odoc_model.Lang.Page.t ->
  Odoc_model.Lang.Page.t Lookup_failures.with_failures
