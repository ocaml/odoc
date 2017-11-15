(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
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

(**/**)

module Attrs = Attrs

module Maps = Maps

module Paths = Paths

module Types = Model

(**/**)

(** {2:from_ocaml Processing OCaml's compilation units} *)

type 'a result =
  | Ok of 'a Model.Unit.t
  | Not_an_interface
  | Wrong_version
  | Corrupted
  | Not_a_typedtree
  | Not_an_implementation

val read_cmti : (string -> Digest.t -> 'a) -> string -> 'a result

val read_cmt : (string -> Digest.t -> 'a) -> string -> 'a result

val read_cmi : (string -> Digest.t -> 'a) -> string -> 'a result

(** {2:resolving Resolving}

    This is the part of DocOck handling the resolving of path and references. *)

type 'a resolver

type 'a lookup_result =
  | Forward_reference
  | Found of { root : 'a; hidden : bool }
  | Not_found

(** Build a resolver. Optionally provide equality and hash on ['a]. *)
val build_resolver : ?equal:('a -> 'a -> bool) -> ?hash:('a -> int)
  -> (string -> 'a lookup_result) -> ('a -> 'a Model.Unit.t)
  -> (string -> 'a option) -> ('a -> 'a Model.Page.t)
  -> 'a resolver

val resolve : 'a resolver -> 'a Model.Unit.t -> 'a Model.Unit.t

val resolve_page : 'a resolver -> 'a Model.Page.t -> 'a Model.Page.t

(** {2:expansion Expansion}

    This is the part of DocOck in charge of performing substitutions, inlining
    of includes, etc. *)

type 'a expander

(** Build an expander. Assumes that it is safe to use {!Hashtbl.hash} and
    structural equality (=) on ['a]. *)
val build_expander : ?equal:('a -> 'a -> bool) -> ?hash:('a -> int)
  -> (string -> 'a lookup_result) -> (root:'a -> 'a -> 'a Model.Unit.t)
  -> 'a expander

val expand : 'a expander -> 'a Model.Unit.t -> 'a Model.Unit.t

(** {2 Misc.}

    OCaml's predefined types and exceptions. *)

val core_types : 'a Model.TypeDecl.t list

val core_exceptions : 'a Model.Exception.t list

module Lookup = Lookup

