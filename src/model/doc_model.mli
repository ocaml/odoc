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

type read_result =
  (Model.Compilation_unit.t, read_error) result

and read_error = private
  | Not_an_interface
  | Wrong_version
  | Corrupted
  | Not_a_typedtree
  | Not_an_implementation

val read_cmti :
  make_root:(module_name:string -> digest:Digest.t -> Root.t) ->
  filename:string ->
    read_result

val read_cmt :
  make_root:(module_name:string -> digest:Digest.t -> Root.t) ->
  filename:string ->
    read_result

val read_cmi :
  make_root:(module_name:string -> digest:Digest.t -> Root.t) ->
  filename:string ->
    read_result

(** {2:resolving Resolving}

    This is the part of DocOck handling the resolving of path and references. *)

type resolver

type lookup_result =
  | Forward_reference
  | Found of { root : Root.t; hidden : bool }
  | Not_found

(** Build a resolver. Optionally provide equality and hash on ['a]. *)
val build_resolver : ?equal:(Root.t -> Root.t -> bool) -> ?hash:(Root.t -> int)
  -> (string -> lookup_result) -> (Root.t -> Model.Compilation_unit.t)
  -> (string -> Root.t option) -> (Root.t -> Model.Page.t)
  -> resolver

val resolve : resolver -> Model.Compilation_unit.t -> Model.Compilation_unit.t

val resolve_page : resolver -> Model.Page.t -> Model.Page.t

(** {2:expansion Expansion}

    This is the part of DocOck in charge of performing substitutions, inlining
    of includes, etc. *)

type expander

(** Build an expander. Assumes that it is safe to use {!Hashtbl.hash} and
    structural equality (=) on ['a]. *)
val build_expander : ?equal:(Root.t -> Root.t -> bool) -> ?hash:(Root.t -> int)
  -> (string -> lookup_result)
  -> (root:Root.t -> Root.t -> Model.Compilation_unit.t)
  -> expander

val expand : expander -> Model.Compilation_unit.t -> Model.Compilation_unit.t

(** {2 Misc.}

    OCaml's predefined types and exceptions. *)

val core_types : Model.TypeDecl.t list

val core_exceptions : Model.Exception.t list

module Lookup = Lookup

module Root = Root
