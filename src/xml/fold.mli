(*
 * Copyright (c) 2014-2015 Leo White <lpw25@cl.cam.ac.uk>
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

type 'a t =
  { f : 'acc. ('acc -> Xmlm.signal -> 'acc) -> 'acc -> 'a -> 'acc }

val text: Doc_model.Root.t t -> Doc_model.Types.Documentation.text t

val unit: Doc_model.Root.t t -> Doc_model.Types.Compilation_unit.t t
val file_unit: Doc_model.Root.t t -> Doc_model.Types.Compilation_unit.t t

val page: Doc_model.Root.t t -> Doc_model.Types.Page.t t
val file_page: Doc_model.Root.t t -> Doc_model.Types.Page.t t
