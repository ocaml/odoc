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
        match cmt_info.cmt_imports with
        | (name, _) :: imports when name = cmt_info.cmt_modname ->
            let module_ = DocOckCmti.read_interface root intf in
            let imports = List.map (fun (s, d) -> Unresolved(s, d)) imports in
              Ok {module_; imports}
        | _ -> Corrupted_interface
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
      | (name, _) :: imports when name = cmi_info.cmi_name ->
          let module_ = DocOckCmi.read_interface root cmi_info.cmi_sign in
          let imports = List.map (fun (s, d) -> Unresolved(s, d)) imports in
            Ok {module_; imports}
      | _ -> Corrupted_interface
  with
  | Cmi_format.Error (Not_an_interface _) -> Not_an_interface
  | Cmi_format.Error (Wrong_version_interface _) -> Wrong_version_interface
  | Cmi_format.Error (Corrupted_interface _) -> Corrupted_interface
