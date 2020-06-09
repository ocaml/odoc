(* Compile *)

val signature :
  Env.t ->
  Odoc_model.Paths.Identifier.Signature.t ->
  Odoc_model.Lang.Signature.t ->
  Odoc_model.Lang.Signature.t

val expansion :
  Env.t ->
  Odoc_model.Paths.Identifier.Signature.t ->
  Odoc_model.Lang.Module.expansion ->
  Odoc_model.Lang.Module.expansion

type msg = [ `Msg of string ]

val build_resolver :
  ?equal:(Odoc_model.Root.t -> Odoc_model.Root.t -> bool) ->
  ?hash:(Odoc_model.Root.t -> int) ->
  string list ->
  (string -> Env.lookup_unit_result) ->
  (Odoc_model.Root.t -> (Odoc_model.Lang.Compilation_unit.t, msg) result) ->
  (string -> Odoc_model.Root.t option) ->
  (Odoc_model.Root.t -> (Odoc_model.Lang.Page.t, msg) result) ->
  Env.resolver

val compile :
  Env.resolver ->
  Odoc_model.Lang.Compilation_unit.t ->
  Odoc_model.Lang.Compilation_unit.t Lookup_failures.with_failures

val resolve_page : 'a -> 'b -> 'b
