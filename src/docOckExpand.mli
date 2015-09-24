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

open DocOckPaths
open DocOckTypes

type 'a module_expansion =
  | Signature of 'a Signature.t
  | Functor of ('a Identifier.module_ *
                'a ModuleType.expr) option list *
               'a Signature.t

type 'a t

val build_expander : ?equal:('a -> 'a -> bool) -> ?hash:('a -> int) ->
                     (root:'a -> 'a -> 'a Unit.t) -> 'a t

val expand_module : 'a t -> 'a Module.t ->
                    'a module_expansion option

val expand_module_type : 'a t -> 'a ModuleType.t ->
                         'a module_expansion option

val expand_unit : 'a t -> 'a Unit.t ->
                  'a Signature.t option

val expand_include : 'a t -> 'a Include.t ->
                     'a Signature.t option
