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
let unit_identifier = Mk.core_type "unit"
let exn_identifier = Mk.core_type "exn"
let list_identifier = Mk.core_type "list"
let option_identifier = Mk.core_type "option"

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

(** The type representation for known core types. *)
let type_repr_of_core_type =
  let open TypeDecl.Representation in
  function
  | "bool" -> Some (Variant [ false_decl; true_decl ])
  | "unit" -> Some (Variant [ void_decl ])
  | "exn" -> Some Extensible
  | "option" -> Some (Variant [ none_decl; some_decl ])
  | "list" -> Some (Variant [ nil_decl; cons_decl ])
  | _ -> None

let type_eq_of_core_type = function
  | "lazy_t" | "extension_constructor" -> Some covariant_equation
  | "array" -> Some invariant_equation
  | _ -> None

let doc_of_core_type =
  let elt x = Location_.at predefined_location x in
  let words ss =
    ss
    |> List.rev_map (fun s -> [ elt `Space; elt (`Word s) ])
    |> List.flatten |> List.tl |> List.rev
  in
  let paragraph x = elt (`Paragraph x) in
  function
  | "floatarray" ->
      Some
        [
          paragraph
            (words [ "This"; "type"; "is"; "used"; "to"; "implement"; "the" ]
            @ [
                elt `Space;
                elt
                  (`Reference
                    ( `Module
                        ( `Root ("Array", `TModule),
                          ModuleName.make_std "Floatarray" ),
                      [] ));
                elt `Space;
              ]
            @ words
                [ "module."; "It"; "should"; "not"; "be"; "used"; "directly." ]
            );
        ]
  | _ -> None

let type_of_core_type name =
  let identifier = Mk.core_type name
  and repr = type_repr_of_core_type name
  and eq = type_eq_of_core_type name
  and doc = doc_of_core_type name in
  mk_type ?doc ?repr ?eq identifier
