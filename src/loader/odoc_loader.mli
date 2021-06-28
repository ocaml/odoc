open Result
open Odoc_model

val read_string :
  Paths.Identifier.LabelParent.t ->
  string ->
  string ->
  (Comment.docs_or_stop, Error.t) result Error.with_warnings

val read_cmti :
  make_root:
    (module_name:string ->
    digest:Digest.t ->
    (Odoc_model.Root.t, [ `Msg of string ]) result) ->
  parent:Odoc_model.Paths.Identifier.ContainerPage.t option ->
  filename:string ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_cmt :
  make_root:
    (module_name:string ->
    digest:Digest.t ->
    (Odoc_model.Root.t, [ `Msg of string ]) result) ->
  parent:Odoc_model.Paths.Identifier.ContainerPage.t option ->
  filename:string ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_cmi :
  make_root:
    (module_name:string ->
    digest:Digest.t ->
    (Odoc_model.Root.t, [ `Msg of string ]) result) ->
  parent:Odoc_model.Paths.Identifier.ContainerPage.t option ->
  filename:string ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings
