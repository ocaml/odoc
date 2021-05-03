(* Compile *)

val signature :
  Env.t ->
  Odoc_model.Paths.Identifier.Signature.t ->
  Odoc_model.Lang.Signature.t ->
  Odoc_model.Lang.Signature.t

val build_resolver :
  string list ->
  (string -> Env.lookup_unit_result) ->
  (string -> Env.lookup_page_result) ->
  Env.resolver

val compile :
  Env.resolver ->
  Odoc_model.Lang.Compilation_unit.t ->
  Odoc_model.Lang.Compilation_unit.t Lookup_failures.with_failures

val resolve_page : 'a -> 'b -> 'b
