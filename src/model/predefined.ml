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

open Lang
open Names

let predefined_location =
  let point = { Location_.line = 1; column = 0 } in
  { Location_.file = "predefined"; start = point; end_ = point }

let empty_doc = []

let mk_equation params =
  let open TypeDecl.Equation in
  { params; private_ = false; manifest = None; constraints = [] }

let nullary_equation = mk_equation []
let covariant_equation =
  mk_equation [ { desc = Var "'a"; variance = Some Pos; injectivity = true } ]
let invariant_equation =
  mk_equation [ { desc = Var "'a"; variance = None; injectivity = true } ]

let locations = None

let mk_type ?(doc = empty_doc) ?(eq = nullary_equation) ?repr id =
  let locs = locations and canonical = None in
  { TypeDecl.id; locs; doc; canonical; equation = eq; representation = repr }

let mk_exn ~args id =
  let locs = locations
  and doc = empty_doc
  and args = TypeDecl.Constructor.Tuple args
  and res = None in
  { Exception.id; locs; doc; args; res }

let mk_constr ?(args = TypeDecl.Constructor.Tuple []) id =
  { TypeDecl.Constructor.id; doc = empty_doc; args; res = None }

module Mk = Paths.Identifier.Mk

let bool_identifier = Mk.core_type "bool"
let int_identifier = Mk.core_type "int"
let char_identifier = Mk.core_type "char"
let bytes_identifier = Mk.core_type "bytes"
let string_identifier = Mk.core_type "string"
let float_identifier = Mk.core_type "float"
let unit_identifier = Mk.core_type "unit"
let exn_identifier = Mk.core_type "exn"
let array_identifier = Mk.core_type "array"
let list_identifier = Mk.core_type "list"
let option_identifier = Mk.core_type "option"
let int32_identifier = Mk.core_type "int32"
let int64_identifier = Mk.core_type "int64"
let nativeint_identifier = Mk.core_type "nativeint"
let lazy_t_identifier = Mk.core_type "lazy_t"
let extension_constructor_identifier = Mk.core_type "extension_constructor"
let floatarray_identifier = Mk.core_type "floatarray"

let false_identifier =
  Mk.constructor (bool_identifier, ConstructorName.make_std "false")

let true_identifier =
  Mk.constructor (bool_identifier, ConstructorName.make_std "true")

let void_identifier =
  Mk.constructor (unit_identifier, ConstructorName.make_std "()")

let nil_identifier =
  Mk.constructor (list_identifier, ConstructorName.make_std "([])")

let cons_identifier =
  Mk.constructor (list_identifier, ConstructorName.make_std "(::)")

let none_identifier =
  Mk.constructor (option_identifier, ConstructorName.make_std "None")

let some_identifier =
  Mk.constructor (option_identifier, ConstructorName.make_std "Some")

let match_failure_identifier = Mk.core_exception "Match_failure"
let assert_failure_identifier = Mk.core_exception "Assert_failure"
let invalid_argument_identifier = Mk.core_exception "Invalid_argument"
let failure_identifier = Mk.core_exception "Failure"
let not_found_identifier = Mk.core_exception "Not_found"
let out_of_memory_identifier = Mk.core_exception "Out_of_memory"
let stack_overflow_identifier = Mk.core_exception "Stack_overflow"
let sys_error_identifier = Mk.core_exception "Sys_error"
let end_of_file_identifier = Mk.core_exception "End_of_file"
let division_by_zero_identifier = Mk.core_exception "Division_by_zero"
let sys_blocked_io_identifier = Mk.core_exception "Sys_blocked_io"

let undefined_recursive_module_identifier =
  Mk.core_exception "Undefined_recursive_module"

let core_type_identifier = function
  | "int" -> Some int_identifier
  | "char" -> Some char_identifier
  | "bytes" -> Some bytes_identifier
  | "string" -> Some string_identifier
  | "float" -> Some float_identifier
  | "bool" -> Some bool_identifier
  | "unit" -> Some unit_identifier
  | "exn" -> Some exn_identifier
  | "array" -> Some array_identifier
  | "list" -> Some list_identifier
  | "option" -> Some option_identifier
  | "int32" -> Some int32_identifier
  | "int64" -> Some int64_identifier
  | "nativeint" -> Some nativeint_identifier
  | "lazy_t" -> Some lazy_t_identifier
  | "extension_constructor" -> Some extension_constructor_identifier
  | "floatarray" -> Some floatarray_identifier
  | _ -> None

let core_exception_identifier = function
  | "Match_failure" -> Some match_failure_identifier
  | "Out_of_memory" -> Some out_of_memory_identifier
  | "Invalid_argument" -> Some invalid_argument_identifier
  | "Failure" -> Some failure_identifier
  | "Not_found" -> Some not_found_identifier
  | "Sys_error" -> Some sys_error_identifier
  | "End_of_file" -> Some end_of_file_identifier
  | "Division_by_zero" -> Some division_by_zero_identifier
  | "Stack_overflow" -> Some stack_overflow_identifier
  | "Sys_blocked_io" -> Some sys_blocked_io_identifier
  | "Assert_failure" -> Some assert_failure_identifier
  | "Undefined_recursive_module" -> Some undefined_recursive_module_identifier
  | _ -> None

let core_constructor_identifier = function
  | "false" -> Some false_identifier
  | "true" -> Some true_identifier
  | "()" -> Some void_identifier
  | "[]" -> Some nil_identifier
  | "([])" -> Some nil_identifier
  | "::" -> Some cons_identifier
  | "(::)" -> Some cons_identifier
  | "None" -> Some none_identifier
  | "Some" -> Some some_identifier
  | _ -> None

let bool_path = `Resolved (`Identifier bool_identifier)
let int_path = `Resolved (`Identifier int_identifier)
let char_path = `Resolved (`Identifier char_identifier)
let bytes_path = `Resolved (`Identifier bytes_identifier)
let string_path = `Resolved (`Identifier string_identifier)
let float_path = `Resolved (`Identifier float_identifier)
let unit_path = `Resolved (`Identifier unit_identifier)
let exn_path = `Resolved (`Identifier exn_identifier)
let array_path = `Resolved (`Identifier array_identifier)
let list_path = `Resolved (`Identifier list_identifier)
let option_path = `Resolved (`Identifier option_identifier)
let int32_path = `Resolved (`Identifier int32_identifier)
let int64_path = `Resolved (`Identifier int64_identifier)
let nativeint_path = `Resolved (`Identifier nativeint_identifier)
let lazy_t_path = `Resolved (`Identifier lazy_t_identifier)

let extension_constructor_path =
  `Resolved (`Identifier extension_constructor_identifier)

let _floatarray_path = `Resolved (`Identifier floatarray_identifier)
let bool_reference = `Resolved (`Identifier bool_identifier)
let int_reference = `Resolved (`Identifier int_identifier)
let char_reference = `Resolved (`Identifier char_identifier)
let bytes_reference = `Resolved (`Identifier bytes_identifier)
let string_reference = `Resolved (`Identifier string_identifier)
let float_reference = `Resolved (`Identifier float_identifier)
let unit_reference = `Resolved (`Identifier unit_identifier)
let exn_reference = `Resolved (`Identifier exn_identifier)
let array_reference = `Resolved (`Identifier array_identifier)
let list_reference = `Resolved (`Identifier list_identifier)
let option_reference = `Resolved (`Identifier option_identifier)
let int32_reference = `Resolved (`Identifier int32_identifier)
let int64_reference = `Resolved (`Identifier int64_identifier)
let nativeint_reference = `Resolved (`Identifier nativeint_identifier)
let lazy_t_reference = `Resolved (`Identifier lazy_t_identifier)

let extension_constructor_reference =
  `Resolved (`Identifier extension_constructor_identifier)

let _floatarray_reference = `Resolved (`Identifier floatarray_identifier)
let false_reference = `Resolved (`Identifier false_identifier)
let true_reference = `Resolved (`Identifier true_identifier)
let void_reference = `Resolved (`Identifier void_identifier)
let nil_reference = `Resolved (`Identifier nil_identifier)
let cons_reference = `Resolved (`Identifier cons_identifier)
let none_reference = `Resolved (`Identifier none_identifier)
let some_reference = `Resolved (`Identifier some_identifier)
let match_failure_reference = `Resolved (`Identifier match_failure_identifier)
let assert_failure_reference = `Resolved (`Identifier assert_failure_identifier)

let invalid_argument_reference =
  `Resolved (`Identifier invalid_argument_identifier)

let failure_reference = `Resolved (`Identifier failure_identifier)
let not_found_reference = `Resolved (`Identifier not_found_identifier)
let out_of_memory_reference = `Resolved (`Identifier out_of_memory_identifier)
let stack_overflow_reference = `Resolved (`Identifier stack_overflow_identifier)
let sys_error_reference = `Resolved (`Identifier sys_error_identifier)
let end_of_file_reference = `Resolved (`Identifier end_of_file_identifier)

let division_by_zero_reference =
  `Resolved (`Identifier division_by_zero_identifier)

let sys_blocked_io_reference = `Resolved (`Identifier sys_blocked_io_identifier)

let undefined_recursive_module_reference =
  `Resolved (`Identifier undefined_recursive_module_identifier)

let string_expr = TypeExpr.Constr (string_path, [])
let int_expr = TypeExpr.Constr (int_path, [])

let false_decl = mk_constr ~args:(Tuple []) false_identifier
let true_decl = mk_constr ~args:(Tuple []) true_identifier
let void_decl = mk_constr ~args:(Tuple []) void_identifier
let nil_decl = mk_constr ~args:(Tuple []) nil_identifier

let cons_decl =
  let head = TypeExpr.Var "'a" in
  let tail = TypeExpr.(Constr (list_path, [ head ])) in
  mk_constr ~args:(Tuple [ head; tail ]) cons_identifier

let none_decl = mk_constr ~args:(Tuple []) none_identifier
let some_decl = mk_constr ~args:(Tuple [ TypeExpr.Var "'a" ]) some_identifier

let int_decl = mk_type int_identifier
let char_decl = mk_type char_identifier
let bytes_decl = mk_type bytes_identifier
let string_decl = mk_type string_identifier
let float_decl = mk_type float_identifier
let bool_decl =
  mk_type ~repr:(Variant [ false_decl; true_decl ]) bool_identifier
let unit_decl = mk_type ~repr:(Variant [ void_decl ]) unit_identifier
let exn_decl = mk_type ~repr:Extensible exn_identifier
let array_decl = mk_type ~eq:invariant_equation array_identifier

let list_decl =
  mk_type ~eq:covariant_equation
    ~repr:(Variant [ nil_decl; cons_decl ])
    list_identifier

let option_decl =
  mk_type ~eq:covariant_equation
    ~repr:(Variant [ none_decl; some_decl ])
    option_identifier

let int32_decl = mk_type int32_identifier
let int64_decl = mk_type int64_identifier
let nativeint_decl = mk_type nativeint_identifier
let lazy_t_decl = mk_type ~eq:covariant_equation lazy_t_identifier
let extension_constructor_decl =
  mk_type ~eq:covariant_equation extension_constructor_identifier

let floatarray_decl =
  let words ss =
    ss
    |> List.rev_map (fun s -> [ `Space; `Word s ])
    |> List.flatten |> List.tl |> List.rev
  in
  let doc =
    [
      `Paragraph
        (words [ "This"; "type"; "is"; "used"; "to"; "implement"; "the" ]
         @ [
             `Space;
             `Reference
               ( `Module
                   (`Root ("Array", `TModule), ModuleName.make_std "Floatarray"),
                 [] );
             `Space;
           ]
         @ words [ "module."; "It"; "should"; "not"; "be"; "used"; "directly." ]
        |> List.map (Location_.at predefined_location));
    ]
    |> List.map (Location_.at predefined_location)
  in
  mk_type ~doc ~eq:covariant_equation floatarray_identifier

let match_failure_decl =
  mk_exn
    ~args:[ TypeExpr.Tuple [ string_expr; int_expr; int_expr ] ]
    match_failure_identifier
let assert_failure_decl =
  mk_exn
    ~args:[ TypeExpr.Tuple [ string_expr; int_expr; int_expr ] ]
    assert_failure_identifier
let invalid_argument_decl =
  mk_exn ~args:[ string_expr ] invalid_argument_identifier
let failure_decl = mk_exn ~args:[ string_expr ] failure_identifier
let not_found_decl = mk_exn ~args:[] not_found_identifier
let out_of_memory_decl = mk_exn ~args:[] out_of_memory_identifier
let stack_overflow_decl = mk_exn ~args:[] stack_overflow_identifier
let sys_error_decl = mk_exn ~args:[ string_expr ] sys_error_identifier
let end_of_file_decl = mk_exn ~args:[] end_of_file_identifier
let division_by_zero_decl = mk_exn ~args:[] division_by_zero_identifier
let sys_blocked_io_decl = mk_exn ~args:[] sys_blocked_io_identifier
let undefined_recursive_module_decl =
  mk_exn
    ~args:[ TypeExpr.Tuple [ string_expr; int_expr; int_expr ] ]
    undefined_recursive_module_identifier

let core_types =
  [
    int_decl;
    char_decl;
    bytes_decl;
    string_decl;
    float_decl;
    bool_decl;
    unit_decl;
    exn_decl;
    array_decl;
    list_decl;
    option_decl;
    int32_decl;
    int64_decl;
    nativeint_decl;
    lazy_t_decl;
    extension_constructor_decl;
    floatarray_decl;
  ]

let core_exceptions =
  [
    match_failure_decl;
    assert_failure_decl;
    invalid_argument_decl;
    failure_decl;
    not_found_decl;
    out_of_memory_decl;
    stack_overflow_decl;
    sys_error_decl;
    end_of_file_decl;
    division_by_zero_decl;
    sys_blocked_io_decl;
    undefined_recursive_module_decl;
  ]
