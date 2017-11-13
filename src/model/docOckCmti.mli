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

val read_interface: 'a -> string -> Typedtree.signature ->
  'a DocOckPaths.Identifier.module_ *
  'a DocOckTypes.Documentation.t *
  'a DocOckTypes.Signature.t

val read_module_type : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature -> int ->
  Typedtree.module_type -> 'a DocOckTypes.ModuleType.expr

val read_value_description : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature ->
  Typedtree.value_description -> 'a DocOckTypes.Signature.item

val read_type_declarations : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature ->
  Typedtree.type_declaration list ->
  'a DocOckTypes.Signature.item list

val read_module_type_declaration : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature ->
  Typedtree.module_type_declaration -> 'a DocOckTypes.ModuleType.t

val read_class_type_declarations : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature ->
  Typedtree.class_type Typedtree.class_infos list ->
  'a DocOckTypes.Signature.item list
