open Odoc_model.Paths.Reference

type module_lookup_result =
  Resolved.Module.t * Cpath.Resolved.module_ * Component.Module.t

val resolve_module_reference : Env.t -> Module.t -> module_lookup_result option

val resolve_reference : Env.t -> t -> Resolved.t option
