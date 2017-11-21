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

type text_element =
  | Raw of string (** Raw text. *)
  | Code of string (** The string is source code. *)
  | PreCode of string (** The string is pre-formatted source code. *)
  | Verbatim of string (** String 'as is'. *)
  | Style of Model.Documentation.style * text (** Text tagged with a style. *)
  | List of text list (** A list. *)
  | Enum of text list (** An enumerated list. *)
  | Newline   (** To force a line break. *)
  | Title of int * Model.Paths.Identifier.label option * text
              (** Style number, optional label, and text. *)
  | Ref of Model.Documentation.reference * text option
    (** A reference to an element. Complete name and kind. An optional
        text can be given to display this text instead of the element name.*)
  | Special_ref of Model.Documentation.special (** Special kinds of reference *)
  | Target of string option * string
    (** (target, code) : to specify code for a specific target format *)

(** [text] is a list of text_elements. The order matters. *)
and text = text_element list

(** Tags *)
type tag =
  | Author of string (** \@author tag *)
  | Version of string (** \@version tag *)
  | See of Model.Documentation.see * text (** \@see tag *)
  | Since of string (** \@since tag *)
  | Before of string * text (** \@before tag *)
  | Deprecated of text (** \@deprecated tag *)
  | Param of string * text (** \@param tag *)
  | Raised_exception of string * text (** \@raise tag *)
  | Return_value of text (** \@return tag *)
  | Inline (** \@inline tag *)
  | Custom of string * text (** custom tag *)
  | Canonical of string (** \@canonical tag *)

(** A special comment *)
type t = text * tag list
