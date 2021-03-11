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

open Odoc_model

module Paths = Odoc_model.Paths


let empty_body = []

let empty : Odoc_model.Comment.docs = empty_body



let load_payload : Parsetree.payload -> string * Location.t = function
  | PStr [{pstr_desc =
      Pstr_eval ({pexp_desc =
#if OCAML_MAJOR = 4 && OCAML_MINOR = 02
        Pexp_constant (Const_string (text, _))
#elif OCAML_MAJOR = 4 && OCAML_MINOR < 11
        Pexp_constant (Pconst_string (text, _))
#else
        Pexp_constant (Pconst_string (text, _, _))
#endif
   ; pexp_loc = loc; _}, _); _}] ->
     (text, loc)
  | _ -> assert false


    let parse_attribute : Parsetree.attribute -> (string * Location.t) option = function
    #if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
      | { attr_name = { Location.txt =
          ("text" | "ocaml.text"); loc = _loc}; attr_payload; _ } -> begin
    #else
      | ({Location.txt =
          ("text" | "ocaml.text"); loc = _loc}, attr_payload) -> begin
    #endif
      Some (load_payload attr_payload)
        end
      | _ -> None

let pad_loc loc =
  { loc.Location.loc_start with pos_cnum = loc.loc_start.pos_cnum + 3 }

let ast_to_comment ~internal_tags parent ast_docs =
  Error.accumulate_warnings (fun warnings ->
      Odoc_model.Semantics.ast_to_comment warnings ~internal_tags
        ~sections_allowed:`All ~parent_of_sections:parent ast_docs)
  |> Error.raise_warnings

let attached internal_tags parent attrs =
  let rec loop acc =
    function
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | {Parsetree.attr_name = { Location.txt =
          ("doc" | "ocaml.doc"); loc = _loc}; attr_payload; _ } :: rest -> begin
#else
    | ({Location.txt =
          ("doc" | "ocaml.doc"); loc = _loc}, attr_payload) :: rest -> begin
#endif
        let str, loc = load_payload attr_payload in
        let ast_docs =
          Octavius.parse_comment ~location:(pad_loc loc) ~text:str
          |> Error.raise_parser_warnings
        in
        loop (List.rev_append ast_docs acc) rest
      end
    | _ :: rest -> loop acc rest
    | [] -> List.rev acc
  in
  let ast_docs = loop [] attrs in
  ast_to_comment ~internal_tags parent ast_docs

let attached_no_tag parent attrs =
  let x, () = attached Semantics.Expect_none parent attrs in
  x

let read_string internal_tags parent location str =
  Odoc_model.Semantics.parse_comment
    ~internal_tags
    ~sections_allowed:`All
    ~containing_definition:parent
    ~location
    ~text:str
  |> Odoc_model.Error.raise_warnings

let read_string_comment internal_tags parent loc str =
  read_string internal_tags parent (pad_loc loc) str

let page parent loc str =
  let doc, () =
    read_string Odoc_model.Semantics.Expect_none parent loc.Location.loc_start
      str
  in
  `Docs doc

let standalone parent (attr : Parsetree.attribute) :
    Odoc_model.Comment.docs_or_stop option =
  match parse_attribute attr with
  | Some ("/*", _loc) -> Some `Stop
  | Some (str, loc) ->
      let doc, () = read_string_comment Semantics.Expect_none parent loc str in
      Some (`Docs doc)
  | _ -> None

let standalone_multiple parent attrs =
  let coms =
    List.fold_left
      (fun acc attr ->
        match standalone parent attr  with
         | None -> acc
         | Some com -> com :: acc)
      [] attrs
  in
    List.rev coms

let extract_top_comment internal_tags ~classify parent items =
  let rec extract ~classify = function
    | hd :: tl as items -> (
        match classify hd with
        | Some (`Attribute attr) -> (
            match parse_attribute attr with
            | Some (text, loc) ->
                let ast_docs =
                  Octavius.parse_comment ~location:(pad_loc loc) ~text
                  |> Error.raise_parser_warnings
                in
                (tl, ast_docs)
            | None -> (items, []))
        | Some `Open ->
            let items, ast_docs = extract ~classify tl in
            (hd :: items, ast_docs)
        | None -> (items, []))
    | [] -> ([], [])
  in
  let items, ast_docs = extract ~classify items in
  let docs, tags =
    ast_to_comment ~internal_tags
      (parent : Paths.Identifier.Signature.t :> Paths.Identifier.LabelParent.t)
      ast_docs
  in
  (items, docs, tags)

let extract_top_comment_class items =
  match items with
  | Lang.ClassSignature.Comment (`Docs doc) :: tl -> (tl, doc)
  | _ -> items, empty
