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



module Paths = Model.Paths



val empty : Model.Comment.docs

val attached :
  Paths.Identifier.label_parent ->
  Parsetree.attributes ->
    Model.Comment.docs

val page :
  Paths.Identifier.label_parent ->
  Location.t ->
  string ->
    Model.Comment.docs_or_stop
(** The parent identifier is used to define labels in the given string (i.e.
    for things like [{1:some_section Some title}]) and the location is used for
    error messages.

    This function is meant to be used to read arbitrary files containing text in
    the ocamldoc syntax. *)

val standalone :
  Paths.Identifier.label_parent ->
  Parsetree.attribute ->
    Model.Comment.docs_or_stop option

val standalone_multiple :
  Paths.Identifier.label_parent ->
  Parsetree.attributes ->
    Model.Comment.docs_or_stop list
