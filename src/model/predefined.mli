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

(** {3 Identifiers} *)

val bool_identifier : 'a Identifier.type_
val int_identifier : 'a Identifier.type_
val char_identifier : 'a Identifier.type_
val bytes_identifier : 'a Identifier.type_
val string_identifier : 'a Identifier.type_
val float_identifier : 'a Identifier.type_
val unit_identifier : 'a Identifier.type_
val exn_identifier : 'a Identifier.type_
val array_identifier : 'a Identifier.type_
val list_identifier : 'a Identifier.type_
val option_identifier : 'a Identifier.type_
val int32_identifier : 'a Identifier.type_
val int64_identifier : 'a Identifier.type_
val nativeint_identifier : 'a Identifier.type_
val lazy_t_identifier : 'a Identifier.type_
val extension_constructor_identifier : 'a Identifier.type_

val false_identifier :'a Identifier.constructor
val true_identifier :'a Identifier.constructor
val void_identifier :'a Identifier.constructor
val nil_identifier :'a Identifier.constructor
val cons_identifier :'a Identifier.constructor
val none_identifier :'a Identifier.constructor
val some_identifier :'a Identifier.constructor

val match_failure_identifier : 'a Identifier.exception_
val assert_failure_identifier : 'a Identifier.exception_
val invalid_argument_identifier : 'a Identifier.exception_
val failure_identifier : 'a Identifier.exception_
val not_found_identifier : 'a Identifier.exception_
val out_of_memory_identifier : 'a Identifier.exception_
val stack_overflow_identifier : 'a Identifier.exception_
val sys_error_identifier : 'a Identifier.exception_
val end_of_file_identifier : 'a Identifier.exception_
val division_by_zero_identifier : 'a Identifier.exception_
val sys_blocked_io_identifier : 'a Identifier.exception_
val undefined_recursive_module_identifier : 'a Identifier.exception_

val core_type_identifier : string ->
      ('a, [< Identifier.kind > `Type]) Identifier.t option
val core_exception_identifier : string ->
      ('a, [< Identifier.kind > `Exception]) Identifier.t option
val core_constructor_identifier : string ->
      ('a, [< Identifier.kind > `Constructor]) Identifier.t option

(** {3 Paths} *)

val bool_path : 'a Path.type_
val int_path : 'a Path.type_
val char_path : 'a Path.type_
val bytes_path : 'a Path.type_
val string_path : 'a Path.type_
val float_path : 'a Path.type_
val unit_path : 'a Path.type_
val exn_path : 'a Path.type_
val array_path : 'a Path.type_
val list_path : 'a Path.type_
val option_path : 'a Path.type_
val int32_path : 'a Path.type_
val int64_path : 'a Path.type_
val nativeint_path : 'a Path.type_
val lazy_t_path : 'a Path.type_
val extension_constructor_path : 'a Path.type_

(** {3 References} *)

val bool_reference : 'a Reference.type_
val int_reference : 'a Reference.type_
val char_reference : 'a Reference.type_
val bytes_reference : 'a Reference.type_
val string_reference : 'a Reference.type_
val float_reference : 'a Reference.type_
val unit_reference : 'a Reference.type_
val exn_reference : 'a Reference.type_
val array_reference : 'a Reference.type_
val list_reference : 'a Reference.type_
val option_reference : 'a Reference.type_
val int32_reference : 'a Reference.type_
val int64_reference : 'a Reference.type_
val nativeint_reference : 'a Reference.type_
val lazy_t_reference : 'a Reference.type_
val extension_constructor_reference : 'a Reference.type_

val false_reference : 'a Reference.constructor
val true_reference : 'a Reference.constructor
val void_reference : 'a Reference.constructor
val nil_reference : 'a Reference.constructor
val cons_reference : 'a Reference.constructor
val none_reference : 'a Reference.constructor
val some_reference : 'a Reference.constructor

val match_failure_reference : 'a Reference.exception_
val assert_failure_reference : 'a Reference.exception_
val invalid_argument_reference : 'a Reference.exception_
val failure_reference : 'a Reference.exception_
val not_found_reference : 'a Reference.exception_
val out_of_memory_reference : 'a Reference.exception_
val stack_overflow_reference : 'a Reference.exception_
val sys_error_reference : 'a Reference.exception_
val end_of_file_reference : 'a Reference.exception_
val division_by_zero_reference : 'a Reference.exception_
val sys_blocked_io_reference : 'a Reference.exception_
val undefined_recursive_module_reference : 'a Reference.exception_

(** {3 Declarations} *)

val int_decl : 'a TypeDecl.t
val char_decl : 'a TypeDecl.t
val bytes_decl : 'a TypeDecl.t
val string_decl : 'a TypeDecl.t
val float_decl : 'a TypeDecl.t
val bool_decl : 'a TypeDecl.t
val unit_decl : 'a TypeDecl.t
val exn_decl : 'a TypeDecl.t
val array_decl : 'a TypeDecl.t
val list_decl : 'a TypeDecl.t
val option_decl : 'a TypeDecl.t
val int32_decl : 'a TypeDecl.t
val int64_decl : 'a TypeDecl.t
val nativeint_decl : 'a TypeDecl.t
val lazy_t_decl : 'a TypeDecl.t
val extension_constructor_decl : 'a TypeDecl.t

val match_failure_decl : 'a Exception.t
val assert_failure_decl : 'a Exception.t
val invalid_argument_decl : 'a Exception.t
val failure_decl : 'a Exception.t
val not_found_decl : 'a Exception.t
val out_of_memory_decl : 'a Exception.t
val stack_overflow_decl : 'a Exception.t
val sys_error_decl : 'a Exception.t
val end_of_file_decl : 'a Exception.t
val division_by_zero_decl : 'a Exception.t
val sys_blocked_io_decl : 'a Exception.t
val undefined_recursive_module_decl : 'a Exception.t

val core_types : 'a TypeDecl.t list
val core_exceptions : 'a Exception.t list
