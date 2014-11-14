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

type 'a result =
  | Ok of 'a Types.Unit.t
  | Not_an_interface
  | Wrong_version_interface
  | Corrupted_interface
  | Not_a_typedtree

val read_cmti: 'a -> string -> 'a result

val read_cmi: 'a -> string -> 'a result

type 'a resolver

(** Build a resolver. [equal] and [hash] are used
    for memoization, they default to [(=)] and [Hashtbl.hash]
    respectively. *)
val build_resolver: ?equal:('a -> 'a -> bool) -> ?hash:('a -> int) ->
  (string -> 'a option) -> ('a -> 'a Types.Unit.t) -> 'a resolver

val resolve: 'a resolver -> 'a Types.Unit.t -> 'a Types.Unit.t

type 'a expander

(** Build an expander. [equal] and [hash] are used
    for memoization, they default to [(=)] and [Hashtbl.hash]
    respectively. *)
val build_expander: ?equal:('a -> 'a -> bool) -> ?hash:('a -> int) ->
  ('a -> 'a Types.Unit.t) -> 'a expander

type 'a expansion =
  | Signature of 'a Types.Signature.t
  | Functor of ('a Paths.Identifier.module_ *
                'a Types.ModuleType.expr) option list *
               'a Types.Signature.t

val expand_module: 'a expander -> 'a Types.Module.decl ->
  'a expansion option

val expand_module_type: 'a expander -> 'a Types.ModuleType.expr ->
  'a expansion option
