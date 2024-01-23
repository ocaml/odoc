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

let exn_path = `Resolved (`Identifier exn_identifier)
let list_path = `Resolved (`Identifier list_identifier)

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
