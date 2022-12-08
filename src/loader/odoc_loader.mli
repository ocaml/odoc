open Result
open Odoc_model
open Odoc_model.Paths

module Lookup_def = Lookup_def
module Source_info = Source_info

type make_root =
  module_name:string ->
  digest:Digest.t ->
  (Odoc_model.Root.t, [ `Msg of string ]) result

val read_string :
  Paths.Identifier.LabelParent.t ->
  string ->
  string ->
  (Comment.docs_or_stop, Error.t) result Error.with_warnings

val read_cmt_infos :
  filename:string ->
  ((Lookup_def.t * Source_info.local_jmp_infos) option, Error.t) result
  Error.with_warnings
(** Read the shape from a .cmt file. *)

val read_cmti :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_cmt :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  ( Lang.Compilation_unit.t * (Lookup_def.t * Source_info.local_jmp_infos) option,
    Error.t )
  result
  Error.with_warnings
(** The shape is not returned in case of a pack. *)

val read_cmi :
  make_root:make_root ->
  parent:Identifier.ContainerPage.t option ->
  filename:string ->
  (Lang.Compilation_unit.t, Error.t) result Error.with_warnings

val read_location : Location.t -> Location_.span
