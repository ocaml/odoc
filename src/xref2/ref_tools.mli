open Odoc_model.Paths.Reference

type module_lookup_result =
  Resolved.Module.t * Cpath.Resolved.module_ * Component.Module.t

type asset_lookup_result = Resolved.Asset.t

type 'a ref_result =
  ('a, Errors.Tools_error.reference_lookup_error) Result.result

val resolve_module_reference :
  Env.t ->
  Module.t ->
  module_lookup_result ref_result Odoc_model.Error.with_warnings

val resolve_asset_reference :
  Env.t ->
  Asset.t ->
  asset_lookup_result ref_result Odoc_model.Error.with_warnings

val resolve_reference :
  Env.t ->
  t ->
  (Resolved.t * Odoc_model.Comment.paragraph option) ref_result
  Odoc_model.Error.with_warnings
