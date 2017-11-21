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

(** {1 } *)

module Package : sig

  type t

  val create : string -> t

  val to_string : t -> string
end

module Odoc_file : sig

  type t =
    | Page of string
    | Compilation_unit of { name : string; hidden : bool }

  val create_unit : force_hidden:bool -> string -> t
  val create_page : string -> t

  val name : t -> string
  val kind : t -> string
end

module Digest = Digest

type t

val equal : t -> t -> bool
val hash  : t -> int

val create : package:Package.t -> file:Odoc_file.t -> digest:Digest.t -> t

(** {1 Accessors} *)

val digest : t -> Digest.t
val package: t -> Package.t
val file   : t -> Odoc_file.t

(** {1 Serialization} *)

val to_string : t -> string
(** Useful for debugging *)

(**/**)

module Table : Hashtbl.S with type key = t
