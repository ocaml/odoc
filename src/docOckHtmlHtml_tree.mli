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

class page_creator :
  ?kind:kind -> path:string list -> Html_types.div_content_fun elt list ->
  object
    val has_parent : bool

    method name : string

    method title_string : string

    method css_url : string

    method header : Html_types.head elt

    method heading : Html_types.h1_content_fun elt list

    method content : Html_types.div_content_fun elt list

    method html : [ `Html ] elt
  end

val set_page_creator
  : (?kind:kind -> path:string list -> Html_types.div_content_fun elt list
     -> page_creator)
  -> unit

val make : Html_types.div_content_fun elt list * t list -> t
(** [make (body, children)] calls "the page creator" to turn [body] into an
    [[ `Html ] elt].
    If [set_page_creator] was not called, a default creator is used. *)

module Relative_link : sig
  val semantic_uris : bool ref
  (** Whether to generate pretty/semantics links or not. *)

  module Id : sig
    exception Not_linkable

    val href : get_package:('a -> string) -> stop_before:bool ->
      ('a, _) Identifier.t -> string
  end

  val of_path : get_package:('a -> string) -> ('a, _) Path.t
    -> [> `A of [> `PCDATA ] | `PCDATA ] elt list

  val of_fragment : get_package:('a -> string) -> base:'a Identifier.signature
    -> ('a, _, Fragment.sort) Fragment.raw
    -> [> `A of [> `PCDATA ] | `PCDATA ] elt list

  val to_sub_element : kind:kind -> string -> [> `Href ] attrib
end

(* TODO: move to a centralized [State] module or something. Along with
   Relative_link.semantic_uris. *)
val open_details : bool ref
(** Whether [<details>] tags should be opened by default or not.
    Default is [true]. *)
