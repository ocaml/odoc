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

type unit_content = Lang.Compilation_unit.t

(** Either a page or a module. *)
type content =
  | Page_content of Lang.Page.t
  | Source_tree_content of Lang.SourceTree.t
  | Unit_content of unit_content

type t = { content : content; warnings : Error.t list }

(** {2 Serialization} *)

val save_page : Fs.File.t -> warnings:Error.t list -> Lang.Page.t -> unit
(** Save a page. The [page-] prefix is added to the file name if missing. *)

val save_source_tree :
  Fs.File.t -> warnings:Error.t list -> Lang.SourceTree.t -> unit
(** Save a source tree page. The [src-] prefix is added to the file name if
    missing. *)

val save_unit : Fs.File.t -> warnings:Error.t list -> unit_content -> unit
(** Save a module. *)

(** {2 Deserialization} *)

val load : Fs.File.t -> (t, [> msg ]) result
(** Load an [.odoc] file. *)

val load_root : Fs.File.t -> (Root.t, [> msg ]) result
(** Only load the root. Faster than {!load}, used for looking up imports. *)
