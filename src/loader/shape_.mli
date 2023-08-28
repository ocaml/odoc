open Odoc_model
open Odoc_model.Paths
#if OCAML_VERSION >= (4, 14, 0)

type t = Shape.t * Odoc_model.Paths.Identifier.SourceLocation.t Shape.Uid.Map.t

#else

type t

#endif

val lookup_def :
  (string -> (Lang.Compilation_unit.t * t) option) ->
  Identifier.NonSrc.t ->
  Identifier.SourceLocation.t option
