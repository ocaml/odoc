(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
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

open Tyxml.Html
open DocOck.Paths

type t = private {
  name : string;
  content : [ `Html ] elt;
  children : t list
}

val traverse
    : f:(parents:string list -> string -> [ `Html ] elt -> unit)
  -> t
  -> unit

type kind = [ `Arg | `Mod | `Mty ]

(** These two functions are used to track the depth while building the tree,
    which is needed to produce correct links. *)

val enter : ?kind:kind -> string -> unit

val leave : unit -> unit

(** {1 Page creator} *)

type 'a page_creator =
  Html_types.div_content_fun elt ->
  kind:kind ->
  path:string list ->
  [ `Html ] elt

val set_page_creator : _ page_creator -> unit

val make : Html_types.div_content_fun elt * t list -> t
(** [make (body, children)] calls "the page creator" to turn [body] into an
    [[ `Html ] elt].
    If [set_page_creator] was not called, a default creator is used. *)

module Relative_link : sig
  val semantic_uris : bool ref
  (** Whether to generate pretty/semantics links or not. *)

  module Id : sig
    val href : get_package:('a -> string) -> stop_before:bool ->
      ('a, _) Identifier.t -> string
  end

  val of_path : get_package:('a -> string) -> ('a, _) Path.t
    -> [> `A of [> `PCDATA ] | `PCDATA ] elt list

  val of_fragment : get_package:('a -> string) -> base:'a Identifier.signature
    -> ('a, _, Fragment.sort) Fragment.raw
    -> [> `A of [> `PCDATA ] | `PCDATA ] elt list

  val of_reference : ('a, _) Reference.t
    -> [> `A of [> `PCDATA ] | `PCDATA ] elt list

  val to_sub_element : kind:kind -> string -> [> `Href ] attrib
end
