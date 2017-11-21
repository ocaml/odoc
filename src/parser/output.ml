(*
 * Copyright (c) 2015 Leo White <leo@lpw25.net>
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

(** Types for the information collected in comments. *)

(** Tags *)
type tag =
  | Author of string (** \@author tag *)
  | Version of string (** \@version tag *)
  | See of Model.Documentation.see * Model.Documentation.text (** \@see tag *)
  | Since of string (** \@since tag *)
  | Before of string * Model.Documentation.text (** \@before tag *)
  | Deprecated of Model.Documentation.text (** \@deprecated tag *)
  | Param of string * Model.Documentation.text (** \@param tag *)
  | Raised_exception of string * Model.Documentation.text (** \@raise tag *)
  | Return_value of Model.Documentation.text (** \@return tag *)
  | Inline (** \@inline tag *)
  | Custom of string * Model.Documentation.text (** custom tag *)
  | Canonical of Model.Paths.Path.module_ * Model.Paths.Reference.module_
    (** \@canonical tag *)

(** A special comment *)
type t = Model.Documentation.text * tag list
