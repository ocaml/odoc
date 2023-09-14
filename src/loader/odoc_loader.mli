open Result
open Odoc_model
open Odoc_model.Paths

type make_root =
  module_name:string ->
  digest:Digest.t ->
  (Odoc_model.Root.t, [ `Msg of string ]) result

val read_string :
  Paths.Identifier.LabelParent.t ->
  string ->
  string ->
  (Comment.docs_or_stop, Error.t) result Error.with_warnings

val read_cmti :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  source_id_opt:Identifier.SourcePage.t option ->
  cmt_filename_opt:string option ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_cmt :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  source_id_opt:Identifier.SourcePage.t option ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_cmi :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_location : Location.t -> Location_.span
