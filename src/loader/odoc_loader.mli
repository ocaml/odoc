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
  (Comment.docs * Frontmatter.t, Error.t) result Error.with_warnings

val read_cmti :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  warnings_tag:string option ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_cmt :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  warnings_tag:string option ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_impl :
  make_root:make_root ->
  filename:string ->
  source_id:Identifier.SourcePage.t option ->
  (Lang.Implementation.t, Error.t) result Error.with_warnings

val read_cmi :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  warnings_tag:string option ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_location : Location.t -> Location_.span

val wrap_errors :
  filename:string ->
  (unit -> 'a) ->
  'a Odoc_model.Error.with_errors_and_warnings

val parse_attribute : Parsetree.attribute -> Doc_attr.parsed_attribute option
