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

module Paths = DocOckPaths

module Types = DocOckTypes

val core_types : 'a Types.TypeDecl.t list

val core_exceptions : 'a Types.Exception.t list

type 'a result =
  | Ok of 'a Types.Unit.t
  | Not_an_interface
  | Wrong_version
  | Corrupted
  | Not_a_typedtree
  | Not_an_implementation

val read_cmti: (string -> Digest.t -> 'a) -> string -> 'a result

val read_cmt: (string -> Digest.t -> 'a) -> string -> 'a result

val read_cmi: (string -> Digest.t -> 'a) -> string -> 'a result

type 'a resolver

(** Build a resolver. Optionally provide equality and hash on ['a]. *)
val build_resolver: ?equal:('a -> 'a -> bool) -> ?hash:('a -> int) ->
                    ('a Types.Unit.t -> string -> 'a option) ->
                    ('a -> 'a Types.Unit.t) -> 'a resolver

val resolve: 'a resolver -> 'a Types.Unit.t -> 'a Types.Unit.t

type 'a expander

(** Build an expander. Assumes that it is safe to use {!Hashtbl.hash} and
    structural equality (=) on ['a]. *)
val build_expander: ?equal:('a -> 'a -> bool) -> ?hash:('a -> int) ->
                    (root:'a -> 'a -> 'a Types.Unit.t) -> 'a expander

type 'a module_expansion =
  | Signature of 'a Types.Signature.t
  | Functor of ('a Paths.Identifier.module_ *
                'a Types.ModuleType.expr) option list *
               'a Types.Signature.t

val expand_module: 'a expander -> 'a Types.Module.t ->
  'a module_expansion option

val expand_module_type: 'a expander -> 'a Types.ModuleType.t ->
  'a module_expansion option

val expand_unit : 'a expander -> 'a Types.Unit.t ->
  'a Types.Signature.t option

val expand_include : 'a expander -> 'a Types.Include.t ->
  'a Types.Signature.t option
