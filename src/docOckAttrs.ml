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

open Documentation
open DocOckTypes.Documentation

module Env = DocOckEnvironment

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let rec add_text_element parent (elem : Documentation.text_element) env =
  match elem with
  | Raw _ | Code _ | PreCode _ | Verbatim _
  | Newline | Special_ref _ | Target _ | Ref(_, _, None) -> env
  | Style(_, txt) | Ref(_, _, Some txt) -> add_text parent txt env
  | List l -> List.fold_right (add_text parent) l env
  | Enum l -> List.fold_right (add_text parent) l env
  | Title(_, l, txt) ->
      let env = add_text parent txt env in
        match l with
        | None -> env
        | Some name -> Env.add_label parent name env

and add_text parent txt env =
  List.fold_right (add_text_element parent) txt env

let add_tag parent (tag: Documentation.tag) env =
  match tag with
  | Author _ | Version _ | Since _ -> env
  | See(_, txt) | Before(_, txt) | Deprecated txt
  | Param(_, txt) | Raised_exception(_, txt)
  | Return_value txt | Custom (_, txt) -> add_text parent txt env

let add_attribute parent attr env =
  let open Parsetree in
  let open Location in
  match attr with
  | ({txt = "doc"}, PDoc(Cinfo(text, tags), _)) ->
      let env = add_text parent text env in
      let env = List.fold_right (add_tag parent) tags env in
        env
  | _ -> env

let add_attributes parent attrs env =
  List.fold_right (add_attribute parent) attrs env

let add_comment parent attr env =
  let open Parsetree in
  let open Location in
    match attr with
    | ({txt = "comment"}, PDoc(Cinfo(text, tags), _)) ->
        let env = add_text parent text env in
        let env = List.fold_right (add_tag parent) tags env in
          env
    | _ -> env

let add_comments parent attrs env =
  List.fold_right (add_comment parent) attrs env

let read_style = function
  | SK_bold -> Bold
  | SK_italic -> Italic
  | SK_emphasize -> Emphasize
  | SK_center -> Center
  | SK_left -> Left
  | SK_right -> Right
  | SK_superscript -> Superscript
  | SK_subscript -> Subscript
  | SK_custom s -> Custom s

let read_reference env rk s =
  match rk with
  | RK_module ->
      Module (Env.Reference.read_module env s)
  | RK_module_type ->
      ModuleType (Env.Reference.read_module_type env s)
  | RK_type ->
      Type (Env.Reference.read_type env s)
  | RK_exception ->
      Exception (Env.Reference.read_exception env s)
  | RK_recfield ->
      Field (Env.Reference.read_field env s)
  | RK_const ->
      Constructor (Env.Reference.read_constructor env s)
  | RK_value ->
      Value (Env.Reference.read_value env s)
  | RK_class ->
      Class (Env.Reference.read_class env s)
  | RK_class_type ->
      ClassType (Env.Reference.read_class_type env s)
  | RK_attribute ->
      InstanceVariable (Env.Reference.read_instance_variable env s)
  | RK_method ->
      Method (Env.Reference.read_method env s)
  | RK_element -> Element (Env.Reference.read_element env s)
  | RK_section ->
      Section (Env.Reference.read_label env s)
  | RK_link -> Link s
  | RK_custom k -> Custom(k, s)

let read_special_reference env = function
  | SRK_module_list(mds) ->
      Modules (List.map (Env.Reference.read_module env) mds)
  | SRK_index_list -> Index

let rec read_text_element env parent
  : Documentation.text_element -> 'a text_element =
  function
  | Raw s -> Raw s
  | Code s -> Code s
  | PreCode s -> PreCode s
  | Verbatim s -> Verbatim s
  | Style(sk, txt) -> Style(read_style sk, read_text env parent txt)
  | List l -> List (List.map (read_text env parent) l)
  | Enum l -> Enum (List.map (read_text env parent) l)
  | Newline -> Newline
  | Title(i, l, txt) -> begin
      let txt = read_text env parent txt in
        match l with
        | None -> Title(i, None, txt)
        | Some name ->
            let id = DocOckPaths.Identifier.Label(parent, name) in
              Title(i, Some id, txt)
    end
  | Ref(rk, s, txt) ->
      Reference(read_reference env rk s, opt_map (read_text env parent) txt)
  | Special_ref srk -> Special (read_special_reference env srk)
  | Target (target, code) -> Target (target, code)

and read_text env parent txt = List.map (read_text_element env parent) txt

let read_see = function
  | See_url s -> Url s
  | See_file s -> File s
  | See_doc s -> Doc s


let read_tag env parent : Documentation.tag -> 'a tag = function
  | Author s -> Author s
  | Version v -> Version v
  | See (r, t) -> See (read_see r, read_text env parent t)
  | Since s -> Since s
  | Before (s, t) -> Before (s, read_text env parent t)
  | Deprecated t -> Deprecated (read_text env parent t)
  | Param (s, t) -> Param (s, read_text env parent t)
  | Raised_exception (s, t) -> Raise (s, read_text env parent t)
  | Return_value t -> Return (read_text env parent t)
  | Custom (s, t) -> Tag (s, read_text env parent t)

let empty = { text = []; tags = []; }

open Location
open Parsetree

let read_attributes env parent attrs =
  let rec loop first acc = function
    | ({txt = "doc"}, PDoc(Cinfo(text, tags), _)) :: rest ->
        let text = read_text env parent text in
        let text = if first then text else Newline :: text in
        let tags = List.map (read_tag env parent) tags in
        let acc =
          { text = acc.text @ text;
            tags = acc.tags @ tags; }
        in
          loop false acc rest
    | _ :: rest -> loop first acc rest
    | [] -> acc
  in
    loop true empty attrs

let read_comment env parent : Parsetree.attribute -> 'a comment option =
  function
  | ({txt = "comment"}, PDoc(Cinfo(text, tags), _)) ->
      let text = read_text env parent text in
      let tags = List.map (read_tag env parent) tags in
        Some (Documentation {text; tags})
  | ({txt = "comment"}, PDoc(Cstop, _)) ->
      Some Stop
  | _ -> None

let read_comments env parent attrs =
  let coms =
    List.fold_left
      (fun acc attr ->
         match read_comment env parent attr  with
         | None -> acc
         | Some com -> com :: acc)
      [] attrs
  in
    List.rev coms
