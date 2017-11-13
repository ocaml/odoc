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

val signature : 'a t -> 'a Signature.t -> 'a Signature.t

val class_signature : 'a t -> 'a ClassSignature.t -> 'a ClassSignature.t

val datatype : 'a t -> 'a TypeDecl.Representation.t ->
               'a TypeDecl.Representation.t

val module_ : 'a t -> 'a Module.t -> 'a Module.t

val module_type : 'a t -> 'a ModuleType.t -> 'a ModuleType.t

val type_decl : 'a t -> 'a TypeDecl.t -> 'a TypeDecl.t

val constructor : 'a t -> 'a TypeDecl.Constructor.t ->
                  'a TypeDecl.Constructor.t

val field : 'a t -> 'a TypeDecl.Field.t -> 'a TypeDecl.Field.t

val extension : 'a t -> 'a Extension.t -> 'a Extension.t

val exception_ : 'a t -> 'a Exception.t -> 'a Exception.t

val value : 'a t -> 'a Value.t -> 'a Value.t

val class_ : 'a t -> 'a Class.t -> 'a Class.t

val class_type : 'a t -> 'a ClassType.t -> 'a ClassType.t

val method_ : 'a t -> 'a Method.t -> 'a Method.t

val instance_variable : 'a t -> 'a InstanceVariable.t ->
                        'a InstanceVariable.t

val comment : 'a t -> 'a Documentation.comment -> 'a Documentation.comment

val documentation : 'a t -> 'a Documentation.t -> 'a Documentation.t

val identifier_module : 'a t -> 'a Identifier.module_ ->
                        'a Identifier.module_

val identifier_signature : 'a t -> 'a Identifier.signature ->
                        'a Identifier.signature

val offset_identifier_signature : 'a t -> 'a Identifier.signature * int ->
                                  'a Identifier.signature * int

val module_type_expr : 'a t -> 'a ModuleType.expr -> 'a ModuleType.expr

val module_expansion : 'a t -> 'a Module.expansion -> 'a Module.expansion

val rename_signature : equal:('a -> 'a -> bool) ->
                       'a Identifier.signature ->
                       'a Identifier.signature ->
                       int -> 'a t

val rename_class_signature : equal:('a -> 'a -> bool) ->
                             'a Identifier.class_signature ->
                             'a Identifier.class_signature ->
                             'a t

val rename_datatype : equal:('a -> 'a -> bool) ->
                      'a Identifier.datatype ->
                      'a Identifier.datatype ->
                      'a t

val prefix : equal:('a -> 'a -> bool) ->
             canonical:('a Path.module_ * 'a Reference.module_) option ->
             'a Identifier.module_ ->
             'a t

val strengthen : 'a Path.Resolved.module_ -> 'a t

val pack : equal:('a -> 'a -> bool) -> hash:('a -> int) ->
           ('a Identifier.module_ * 'a Identifier.module_) list -> 'a t
