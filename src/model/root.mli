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

(** A root can be seen as a unique representative of a odoc file.

    {{!t}Roots} are used by doc-ock (at the root of every resolved
    path/identifier/reference) and present at the beginning of every [.odoc]
    file.
*)

module Package : sig
  type t = string
end

module Odoc_file : sig
  type compilation_unit = { name : string; hidden : bool }

  type page = { name : string; title : Comment.link_content option }

  type t =
    | Page of page
    | Compilation_unit of compilation_unit
    | Impl of string

  val create_unit : force_hidden:bool -> string -> t

  val create_page : string -> Comment.link_content option -> t

  val create_impl : string -> t

  val name : t -> string

  val hidden : t -> bool
end

type t = {
  id : Paths.Identifier.OdocId.t;
  file : Odoc_file.t;
  digest : Digest.t;
}

val equal : t -> t -> bool

val hash : t -> int

val compare : t -> t -> int

val to_string : t -> string

module Hash_table : Hashtbl.S with type key = t
