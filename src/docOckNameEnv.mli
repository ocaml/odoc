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

type 'a t

val empty : 'a t

val add_unit : 'a Unit.t -> 'a t -> 'a t

val add_signature_items : 'a Signature.t -> 'a t -> 'a t

val add_module_type_expr_items : 'a ModuleType.expr -> 'a t -> 'a t

val add_module_decl_items : 'a Module.decl -> 'a t -> 'a t

val add_class_signature_items : 'a ClassSignature.t -> 'a t -> 'a t

val add_class_type_expr_items : 'a ClassType.expr -> 'a t -> 'a t

val add_class_decl_items : 'a Class.decl -> 'a t -> 'a t

val lookup_module : 'a t -> 'a Reference.module_ ->
  'a Reference.module_

val lookup_module_type : 'a t -> 'a Reference.module_type ->
  'a Reference.module_type

val lookup_type : 'a t -> 'a Reference.type_ ->
  'a Reference.type_

val lookup_constructor : 'a t -> 'a Reference.constructor ->
  'a Reference.constructor

val lookup_field : 'a t -> 'a Reference.field ->
  'a Reference.field

val lookup_extension : 'a t -> 'a Reference.extension ->
  'a Reference.extension

val lookup_exception : 'a t -> 'a Reference.exception_ ->
  'a Reference.exception_

val lookup_value : 'a t -> 'a Reference.value ->
  'a Reference.value

val lookup_class : 'a t -> 'a Reference.class_ ->
  'a Reference.class_

val lookup_class_type : 'a t -> 'a Reference.class_type ->
  'a Reference.class_type

val lookup_method : 'a t -> 'a Reference.method_ ->
  'a Reference.method_

val lookup_instance_variable : 'a t -> 'a Reference.instance_variable ->
  'a Reference.instance_variable

val lookup_label : 'a t -> 'a Reference.label ->
  'a Reference.label

val lookup_element : 'a t -> 'a Reference.any ->
  'a Reference.any
