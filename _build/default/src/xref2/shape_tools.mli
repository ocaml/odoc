# 1 "shape_tools.cppo.mli"
open Odoc_model.Paths

# 4 "shape_tools.cppo.mli"
type t = Shape.t * Odoc_model.Paths.Identifier.SourceLocation.t Shape.Uid.Map.t


# 12 "shape_tools.cppo.mli"
val lookup_def :
  Env.t ->
  Identifier.NonSrc.t ->
  Identifier.SourceLocation.t option

val lookup_value_path :
  Env.t ->
  Path.Value.t ->
  Identifier.SourceLocation.t option

val lookup_type_path :
  Env.t ->
  Path.Type.t ->
  Identifier.SourceLocation.t option

val lookup_module_path :
  Env.t ->
  Path.Module.t ->
  Identifier.SourceLocation.t option

val lookup_module_type_path :
  Env.t ->
  Path.ModuleType.t ->
  Identifier.SourceLocation.t option

val lookup_class_type_path :
  Env.t ->
  Path.ClassType.t ->
  Identifier.SourceLocation.t option

