open Result
open Odoc_model
open Odoc_model.Paths

type typing_env = {
  uid_to_loc : Location.t Shape.Uid.Tbl.t;
  impl_shape : Shape.t;
}
(** Can only be read from a [.cmt]. *)

type make_root =
  module_name:string ->
  digest:Digest.t ->
  (Odoc_model.Root.t, [ `Msg of string ]) result

val read_string :
  Paths.Identifier.LabelParent.t ->
  string ->
  string ->
  (Comment.docs_or_stop, Error.t) result Error.with_warnings

val read_typing_env :
  filename:string -> (typing_env option, Error.t) result Error.with_warnings
(** Returns [None] if shapes are not supported. *)

val read_cmti :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_cmt :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  (Lang.Compilation_unit.t * typing_env option, Error.t) result
  Error.with_warnings
(** Typing environment is not returned in case of a pack. *)

val read_cmi :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_location : Location.t -> Location_.span
