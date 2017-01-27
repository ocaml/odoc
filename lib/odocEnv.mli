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

type t
type builder

val create :
  ?important_digests:bool -> directories:(OdocFs.Directory.t list) -> builder
(** Do stuff based on a {!Fs.Directory.t} *)

val build : builder -> OdocUnit.t -> t
(** bllblblbl *)

val update_root_unit : t -> OdocUnit.t -> unit

val resolver : t -> OdocRoot.t DocOck.resolver
(** Get a resolver from an env *)

val expander : t -> OdocRoot.t DocOck.expander
(** Get an expander from an env *)

(* val forward_resolver : t -> Root.t DocOck.forward_resolver *)
