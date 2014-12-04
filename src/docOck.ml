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

module Paths = DocOckPaths

module Types = DocOckTypes

let core_types = DocOckPredef.core_types

let core_exceptions = DocOckPredef.core_exceptions

type 'a result =
  | Ok of 'a Types.Unit.t
  | Not_an_interface
  | Wrong_version_interface
  | Corrupted_interface
  | Not_a_typedtree

let read_cmti root filename =
  let open Cmi_format in
  let open Cmt_format in
  let open Types.Unit in
  try
    let cmt_info = read_cmt filename in
    match cmt_info.cmt_annots with
    | Interface intf -> begin
        let (id, doc, items) = DocOckCmti.read_interface root intf in
        let imports =
          List.filter (fun (name, _) -> name <> cmt_info.cmt_modname) cmt_info.cmt_imports
        in
        let imports = List.map (fun (s, d) -> Unresolved(s, d)) imports in
          match cmt_info.cmt_interface_digest with
          | Some digest ->
              Ok {id; doc; digest; imports; items}
          | None -> Corrupted_interface
      end
    | _ -> Not_an_interface
  with
  | Cmi_format.Error (Not_an_interface _) -> Not_an_interface
  | Cmi_format.Error (Wrong_version_interface _) -> Wrong_version_interface
  | Cmi_format.Error (Corrupted_interface _) -> Corrupted_interface
  | Cmt_format.Error (Not_a_typedtree _) -> Not_a_typedtree

let read_cmi root filename =
  let open Cmi_format in
  let open Types.Unit in
  try
    let cmi_info = read_cmi filename in
      match cmi_info.cmi_crcs with
      | (name, Some digest) :: imports when name = cmi_info.cmi_name ->
          let (id, doc, items) = DocOckCmi.read_interface root cmi_info.cmi_sign in
          let imports = List.map (fun (s, d) -> Unresolved(s, d)) imports in
            Ok {id; doc; digest; imports; items}
      | _ -> Corrupted_interface
  with
  | Cmi_format.Error (Not_an_interface _) -> Not_an_interface
  | Cmi_format.Error (Wrong_version_interface _) -> Wrong_version_interface
  | Cmi_format.Error (Corrupted_interface _) -> Corrupted_interface

type 'a resolver = 'a DocOckResolve.resolver

let build_resolver = DocOckResolve.build_resolver

let resolve = DocOckResolve.resolve

type 'a expander = unit

let build_expander fetch = ()

type 'a expansion =
  | Signature of 'a Types.Signature.t
  | Functor of ('a Paths.Identifier.module_ *
                'a Types.ModuleType.expr) option list *
               'a Types.Signature.t

let rec expand_module_type () = function
  | Types.ModuleType.Path _ -> None
  | Types.ModuleType.Signature sg -> Some (Signature sg)
  | Types.ModuleType.Functor(arg, expr) -> begin
      match expand_module_type () expr with
      | None -> None
      | Some (Signature sg) -> Some(Functor([arg], sg))
      | Some (Functor(args, sg)) -> Some(Functor(arg :: args, sg))
    end
  | Types.ModuleType.With _ -> None
  | Types.ModuleType.TypeOf decl -> expand_module () decl

and expand_module () = function
  | Types.Module.Alias _ -> None
  | Types.Module.ModuleType expr -> expand_module_type () expr
