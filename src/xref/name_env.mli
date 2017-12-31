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

open Model
open Paths

type t

val empty : t

val add_page : Lang.Page.t -> t -> t

val add_unit : Lang.Compilation_unit.t -> t -> t

val add_signature_items : Lang.Signature.t -> t -> t

val add_module_type_expr_items : Lang.ModuleType.expr -> t -> t

val add_module_decl_items : Lang.Module.decl -> t -> t

val add_class_signature_items : Lang.ClassSignature.t -> t -> t

val add_class_type_expr_items : Lang.ClassType.expr -> t -> t

val add_class_decl_items : Lang.Class.decl -> t -> t

val lookup_module : t -> Reference.module_ -> Reference.module_

val lookup_module_type : t -> Reference.module_type -> Reference.module_type

val lookup_type : t -> Reference.type_ -> Reference.type_

val lookup_constructor : t -> Reference.constructor -> Reference.constructor

val lookup_field : t -> Reference.field -> Reference.field

val lookup_extension : t -> Reference.extension -> Reference.extension

val lookup_exception : t -> Reference.exception_ -> Reference.exception_

val lookup_value : t -> Reference.value -> Reference.value

val lookup_class : t -> Reference.class_ -> Reference.class_

val lookup_class_type : t -> Reference.class_type -> Reference.class_type

val lookup_method : t -> Reference.method_ -> Reference.method_

val lookup_instance_variable : t -> Reference.instance_variable ->
  Reference.instance_variable

val lookup_label : t -> Reference.label -> Reference.label

val lookup_element : t -> Reference.any -> Reference.any

val lookup_section_title : t -> Reference.Resolved.label ->
  Model.Comment.link_content option
