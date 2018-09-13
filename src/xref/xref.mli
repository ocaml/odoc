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

(** {2:resolving Resolving}

    This is the part of DocOck handling the resolving of path and references. *)

type resolver

type lookup_result_found = { root : Model.Root.t; hidden : bool }

type lookup_result =
  | Forward_reference
  | Found of lookup_result_found
  | Not_found

(** Build a resolver. Optionally provide equality and hash on ['a]. *)
val build_resolver :
  ?equal:(Model.Root.t -> Model.Root.t -> bool) -> ?hash:(Model.Root.t -> int)
  -> (string -> lookup_result)
  -> (Model.Root.t -> Model.Lang.Compilation_unit.t)
  -> (string -> Model.Root.t option) -> (Model.Root.t -> Model.Lang.Page.t)
  -> resolver

val resolve :
  resolver -> Model.Lang.Compilation_unit.t -> Model.Lang.Compilation_unit.t

val resolve_page : resolver -> Model.Lang.Page.t -> Model.Lang.Page.t

(** {2:expansion Expansion}

    This is the part of DocOck in charge of performing substitutions, inlining
    of includes, etc. *)

type expander

(** Build an expander. Assumes that it is safe to use {!Hashtbl.hash} and
    structural equality (=) on ['a]. *)
val build_expander :
  ?equal:(Model.Root.t -> Model.Root.t -> bool) -> ?hash:(Model.Root.t -> int)
  -> (string -> lookup_result)
  -> (root:Model.Root.t -> Model.Root.t -> Model.Lang.Compilation_unit.t)
  -> expander

val expand :
  expander -> Model.Lang.Compilation_unit.t -> Model.Lang.Compilation_unit.t

(** {2 Misc.}

    OCaml's predefined types and exceptions. *)

val core_types : Model.Lang.TypeDecl.t list

val core_exceptions : Model.Lang.Exception.t list

module Lookup = Lookup
