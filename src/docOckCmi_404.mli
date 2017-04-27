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


val read_interface: 'a -> string -> Types.signature ->
  'a DocOckPaths.Identifier.module_ *
  'a DocOckTypes.Documentation.t *
  'a DocOckTypes.Signature.t

val read_label : Asttypes.arg_label -> DocOckTypes.TypeExpr.label option

val mark_type_expr : Types.type_expr -> unit

val read_type_expr : 'a DocOckIdentEnv.t ->
                     Types.type_expr -> 'a DocOckTypes.TypeExpr.t

val mark_type_extension : Types.type_expr list ->
                          Types.extension_constructor list ->
                          Types.type_expr list

val read_type_parameter : bool -> Types.Variance.t ->
                          Types.type_expr -> DocOckTypes.TypeDecl.param

val mark_class_declaration : Types.class_declaration -> unit

val read_self_type : 'a DocOckIdentEnv.t ->
                     Types.type_expr -> 'a DocOckTypes.TypeExpr.t option

val read_type_constraints : 'a DocOckIdentEnv.t -> Types.type_expr list ->
                            ('a DocOckTypes.TypeExpr.t
                             * 'a DocOckTypes.TypeExpr.t) list

val read_class_signature : 'a DocOckIdentEnv.t ->
                           'a DocOckPaths.Identifier.class_signature ->
                           Types.type_expr list -> Types.class_type ->
                           'a DocOckTypes.ClassType.expr

val read_class_type : 'a DocOckIdentEnv.t ->
                      'a DocOckPaths.Identifier.class_signature ->
                      Types.type_expr list -> Types.class_type ->
                      'a DocOckTypes.Class.decl

val read_module_type : 'a DocOckIdentEnv.t ->
                       'a DocOckPaths.Identifier.signature -> int ->
                       Types.module_type -> 'a DocOckTypes.ModuleType.expr

val read_signature : 'a DocOckIdentEnv.t ->
                     'a DocOckPaths.Identifier.signature ->
                     Types.signature -> 'a DocOckTypes.Signature.t

val read_extension_constructor : 'a DocOckIdentEnv.t ->
                       'a DocOckPaths.Identifier.signature ->
                       Ident.t -> Types.extension_constructor ->
                       'a DocOckTypes.Extension.Constructor.t

val read_exception : 'a DocOckIdentEnv.t ->
  'a DocOckPaths.Identifier.signature -> Ident.t ->
  Types.extension_constructor -> 'a DocOckTypes.Exception.t
