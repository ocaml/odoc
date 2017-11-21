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

open Paths
open Model

type t

val signature : t -> Signature.t -> Signature.t

val class_signature : t -> ClassSignature.t -> ClassSignature.t

val datatype : t -> TypeDecl.Representation.t ->
               TypeDecl.Representation.t

val module_ : t -> Module.t -> Module.t

val module_type : t -> ModuleType.t -> ModuleType.t

val type_decl : t -> TypeDecl.t -> TypeDecl.t

val constructor : t -> TypeDecl.Constructor.t ->
                  TypeDecl.Constructor.t

val field : t -> TypeDecl.Field.t -> TypeDecl.Field.t

val extension : t -> Extension.t -> Extension.t

val exception_ : t -> Exception.t -> Exception.t

val value : t -> Value.t -> Value.t

val class_ : t -> Class.t -> Class.t

val class_type : t -> ClassType.t -> ClassType.t

val method_ : t -> Method.t -> Method.t

val instance_variable : t -> InstanceVariable.t ->
                        InstanceVariable.t

val comment : t -> Documentation.comment -> Documentation.comment

val documentation : t -> Documentation.t -> Documentation.t

val identifier_module : t -> Identifier.module_ ->
                        Identifier.module_

val identifier_signature : t -> Identifier.signature ->
                        Identifier.signature

val offset_identifier_signature : t -> Identifier.signature * int ->
                                  Identifier.signature * int

val module_type_expr : t -> ModuleType.expr -> ModuleType.expr

val module_expansion : t -> Module.expansion -> Module.expansion

val rename_signature : equal:(Root.t -> Root.t -> bool) ->
                       Identifier.signature ->
                       Identifier.signature ->
                       int -> t

val rename_class_signature : equal:(Root.t -> Root.t -> bool) ->
                             Identifier.class_signature ->
                             Identifier.class_signature ->
                             t

val rename_datatype : equal:(Root.t -> Root.t -> bool) ->
                      Identifier.datatype ->
                      Identifier.datatype ->
                      t

val prefix : equal:(Root.t -> Root.t -> bool) ->
             canonical:(Path.module_ * Reference.module_) option ->
             Identifier.module_ ->
             t

val strengthen : Path.Resolved.module_ -> t

val pack : equal:(Root.t -> Root.t -> bool) -> hash:(Root.t -> int) ->
           (Identifier.module_ * Identifier.module_) list -> t
