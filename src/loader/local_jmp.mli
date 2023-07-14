open Odoc_model
open Paths

type t

val of_cmt :
  Odoc_model.Paths.Identifier.SourcePage.t option ->
  Odoc_model.Paths.Identifier.RootModule.t ->
  Cmt_format.cmt_infos ->
  t option * Odoc_model.Lang.Source_info.infos

val lookup_def :
  (string -> (Lang.Compilation_unit.t * t) option) ->
  Identifier.NonSrc.t ->
  Identifier.SourceLocation.t option

val anchor_of_uid : t -> Shape.Uid.t -> Identifier.SourceLocation.t option
