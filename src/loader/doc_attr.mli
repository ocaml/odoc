(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Odoc_model
module Paths = Odoc_model.Paths

val empty : bool -> Odoc_model.Comment.docs

val is_stop_comment : Parsetree.attribute -> bool

val attached :
  suppress_warnings:bool ->
  'tags Semantics.handle_internal_tags ->
  Paths.Identifier.LabelParent.t ->
  Parsetree.attributes ->
  Odoc_model.Comment.docs * 'tags

val attached_no_tag :
  suppress_warnings:bool ->
  Paths.Identifier.LabelParent.t ->
  Parsetree.attributes ->
  Odoc_model.Comment.docs
(** Shortcut for [attached Semantics.Expect_none]. *)

val page :
  Paths.Identifier.LabelParent.t ->
  Location.t ->
  string ->
  Odoc_model.Comment.docs * Frontmatter.t
(** The parent identifier is used to define labels in the given string (i.e. for
    things like [{1:some_section Some title}]) and the location is used for
    error messages.

    This function is meant to be used to read arbitrary files containing text in
    the ocamldoc syntax. *)

val standalone :
  Paths.Identifier.LabelParent.t ->
  suppress_warnings:bool ->
  Parsetree.attribute ->
  Odoc_model.Comment.docs_or_stop option

val standalone_multiple :
  Paths.Identifier.LabelParent.t ->
  suppress_warnings:bool ->
  Parsetree.attributes ->
  Odoc_model.Comment.docs_or_stop list

val extract_top_comment :
  'tags Semantics.handle_internal_tags ->
  suppress_warnings:bool ->
  classify:('item -> [ `Attribute of Parsetree.attribute | `Open ] option) ->
  Paths.Identifier.Signature.t ->
  'item list ->
  'item list * (Comment.docs * Comment.docs) * 'tags
(** Extract the first comment of a signature. Returns the remaining items.
    Splits the docs on the first heading *)

val extract_top_comment_class :
  Lang.ClassSignature.item list ->
  Lang.ClassSignature.item list * (Comment.docs * Comment.docs)
(** Extract the first comment of a class signature. Returns the remaining items.
*)

val read_location : Location.t -> Odoc_model.Location_.span

val conv_canonical_module : Odoc_model.Reference.path -> Paths.Path.Module.t
val conv_canonical_type : Odoc_model.Reference.path -> Paths.Path.Type.t option
val conv_canonical_module_type :
  Odoc_model.Reference.path -> Paths.Path.ModuleType.t option
