(* Compile *)

val signature :
  Env.t ->
  Odoc_model.Paths.Identifier.Signature.t ->
  Odoc_model.Lang.Signature.t ->
  Odoc_model.Lang.Signature.t

val compile :
  Env.t ->
  Odoc_model.Lang.Compilation_unit.t ->
  Odoc_model.Lang.Compilation_unit.t Lookup_failures.with_failures

val resolve_page : 'a -> 'b -> 'b
