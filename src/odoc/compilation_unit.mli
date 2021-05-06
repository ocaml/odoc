(*
 * Copyright (c) 2014 Leo White <leo@lpw25.net>
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

(** Load and save [.odoc] and [.odocl] files. *)

open Odoc_model
open Or_error

(** Either a page or a module. *)
type t =
  | Page_content of Lang.Page.t
  | Module_content of Lang.Compilation_unit.t

(** {2 Serialization} *)

val save_page : Fs.File.t -> Lang.Page.t -> unit
(** Save a page. The [page-] prefix is added to the file name if missing. *)

val save_module : Fs.File.t -> Lang.Compilation_unit.t -> unit
(** Save a module. *)

(** {2 Deserialization} *)

val load : Fs.File.t -> (t, [> msg ]) result
(** Load an [.odoc] file. *)
