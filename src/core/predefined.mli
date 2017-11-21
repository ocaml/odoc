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

val bool_identifier : Identifier.type_
val int_identifier : Identifier.type_
val char_identifier : Identifier.type_
val bytes_identifier : Identifier.type_
val string_identifier : Identifier.type_
val float_identifier : Identifier.type_
val unit_identifier : Identifier.type_
val exn_identifier : Identifier.type_
val array_identifier : Identifier.type_
val list_identifier : Identifier.type_
val option_identifier : Identifier.type_
val int32_identifier : Identifier.type_
val int64_identifier : Identifier.type_
val nativeint_identifier : Identifier.type_
val lazy_t_identifier : Identifier.type_
val extension_constructor_identifier : Identifier.type_

val false_identifier : Identifier.constructor
val true_identifier : Identifier.constructor
val void_identifier : Identifier.constructor
val nil_identifier : Identifier.constructor
val cons_identifier : Identifier.constructor
val none_identifier : Identifier.constructor
val some_identifier : Identifier.constructor

val match_failure_identifier : Identifier.exception_
val assert_failure_identifier : Identifier.exception_
val invalid_argument_identifier : Identifier.exception_
val failure_identifier : Identifier.exception_
val not_found_identifier : Identifier.exception_
val out_of_memory_identifier : Identifier.exception_
val stack_overflow_identifier : Identifier.exception_
val sys_error_identifier : Identifier.exception_
val end_of_file_identifier : Identifier.exception_
val division_by_zero_identifier : Identifier.exception_
val sys_blocked_io_identifier : Identifier.exception_
val undefined_recursive_module_identifier : Identifier.exception_

val core_type_identifier : string ->
      [< Identifier.kind > `Type] Identifier.t option
val core_exception_identifier : string ->
      [< Identifier.kind > `Exception] Identifier.t option
val core_constructor_identifier : string ->
      [< Identifier.kind > `Constructor] Identifier.t option

(** {3 Paths} *)

val bool_path : Path.type_
val int_path : Path.type_
val char_path : Path.type_
val bytes_path : Path.type_
val string_path : Path.type_
val float_path : Path.type_
val unit_path : Path.type_
val exn_path : Path.type_
val array_path : Path.type_
val list_path : Path.type_
val option_path : Path.type_
val int32_path : Path.type_
val int64_path : Path.type_
val nativeint_path : Path.type_
val lazy_t_path : Path.type_
val extension_constructor_path : Path.type_

(** {3 References} *)

val bool_reference : Reference.type_
val int_reference : Reference.type_
val char_reference : Reference.type_
val bytes_reference : Reference.type_
val string_reference : Reference.type_
val float_reference : Reference.type_
val unit_reference : Reference.type_
val exn_reference : Reference.type_
val array_reference : Reference.type_
val list_reference : Reference.type_
val option_reference : Reference.type_
val int32_reference : Reference.type_
val int64_reference : Reference.type_
val nativeint_reference : Reference.type_
val lazy_t_reference : Reference.type_
val extension_constructor_reference : Reference.type_

val false_reference : Reference.constructor
val true_reference : Reference.constructor
val void_reference : Reference.constructor
val nil_reference : Reference.constructor
val cons_reference : Reference.constructor
val none_reference : Reference.constructor
val some_reference : Reference.constructor

val match_failure_reference : Reference.exception_
val assert_failure_reference : Reference.exception_
val invalid_argument_reference : Reference.exception_
val failure_reference : Reference.exception_
val not_found_reference : Reference.exception_
val out_of_memory_reference : Reference.exception_
val stack_overflow_reference : Reference.exception_
val sys_error_reference : Reference.exception_
val end_of_file_reference : Reference.exception_
val division_by_zero_reference : Reference.exception_
val sys_blocked_io_reference : Reference.exception_
val undefined_recursive_module_reference : Reference.exception_

(** {3 Declarations} *)

val int_decl : TypeDecl.t
val char_decl : TypeDecl.t
val bytes_decl : TypeDecl.t
val string_decl : TypeDecl.t
val float_decl : TypeDecl.t
val bool_decl : TypeDecl.t
val unit_decl : TypeDecl.t
val exn_decl : TypeDecl.t
val array_decl : TypeDecl.t
val list_decl : TypeDecl.t
val option_decl : TypeDecl.t
val int32_decl : TypeDecl.t
val int64_decl : TypeDecl.t
val nativeint_decl : TypeDecl.t
val lazy_t_decl : TypeDecl.t
val extension_constructor_decl : TypeDecl.t

val match_failure_decl : Exception.t
val assert_failure_decl : Exception.t
val invalid_argument_decl : Exception.t
val failure_decl : Exception.t
val not_found_decl : Exception.t
val out_of_memory_decl : Exception.t
val stack_overflow_decl : Exception.t
val sys_error_decl : Exception.t
val end_of_file_decl : Exception.t
val division_by_zero_decl : Exception.t
val sys_blocked_io_decl : Exception.t
val undefined_recursive_module_decl : Exception.t

val core_types : TypeDecl.t list
val core_exceptions : Exception.t list
