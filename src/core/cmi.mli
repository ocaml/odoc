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


val read_interface: Model.Root.t -> string -> Types.signature ->
  Paths.Identifier.module_ *
  Model.Documentation.t *
  Model.Signature.t

val read_label : Asttypes.arg_label -> Model.TypeExpr.label option

val mark_type_expr : Types.type_expr -> unit

val read_type_expr : Ident_env.t ->
                     Types.type_expr -> Model.TypeExpr.t

val mark_type_extension : Types.type_expr list ->
                          Types.extension_constructor list ->
                          Types.type_expr list

val read_type_parameter : bool -> Types.Variance.t ->
                          Types.type_expr -> Model.TypeDecl.param

val mark_class_declaration : Types.class_declaration -> unit

val read_self_type : Types.type_expr -> Model.TypeExpr.t option

val read_type_constraints : Ident_env.t -> Types.type_expr list ->
                            (Model.TypeExpr.t
                             * Model.TypeExpr.t) list

val read_class_signature : Ident_env.t ->
                           Paths.Identifier.class_signature ->
                           Types.type_expr list -> Types.class_type ->
                           Model.ClassType.expr

val read_class_type : Ident_env.t ->
                      Paths.Identifier.class_signature ->
                      Types.type_expr list -> Types.class_type ->
                      Model.Class.decl

val read_module_type : Ident_env.t ->
                       Paths.Identifier.signature -> int ->
                       Types.module_type -> Model.ModuleType.expr

val read_signature : Ident_env.t ->
                     Paths.Identifier.signature ->
                     Types.signature -> Model.Signature.t

val read_extension_constructor : Ident_env.t ->
                       Paths.Identifier.signature ->
                       Ident.t -> Types.extension_constructor ->
                       Model.Extension.Constructor.t

val read_exception : Ident_env.t ->
  Paths.Identifier.signature -> Ident.t ->
  Types.extension_constructor -> Model.Exception.t
