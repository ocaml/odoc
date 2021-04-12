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

open Result
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

let attached parent attrs =
  let rec loop acc internal_tags =
    function
#if OCAML_MAJOR = 4 && OCAML_MINOR >= 08
    | {Parsetree.attr_name = { Location.txt =
          ("doc" | "ocaml.doc"); loc = _loc}; attr_payload; _ } :: rest -> begin
#else
    | ({Location.txt =
          ("doc" | "ocaml.doc"); loc = _loc}, attr_payload) :: rest -> begin
#endif
        match load_payload attr_payload with
        | (str, loc) -> begin
            let start_pos = loc.Location.loc_start in
            let start_pos =
              {start_pos with pos_cnum = start_pos.pos_cnum + 3} in
            let parsed, parsed_internal_tags =
              Odoc_model.Semantics.parse_comment
                ~sections_allowed:`All
                ~containing_definition:parent
                ~location:start_pos
                ~text:str
              |> Odoc_model.Error.raise_warnings
            in
            loop (acc @ parsed) (internal_tags @ parsed_internal_tags) rest
          end
      end
    | _ :: rest -> loop acc internal_tags rest
    | [] -> Ok (acc, internal_tags)
  in
  loop empty_body [] attrs
  |> Odoc_model.Error.to_exception

let read_string parent loc str =
  let start_pos = loc.Location.loc_start in
  Odoc_model.Semantics.parse_comment
    ~sections_allowed:`All
    ~containing_definition:parent
    ~location:start_pos
    ~text:str
  |> Odoc_model.Error.raise_warnings

let read_string_comment parent loc str =
  let loc_start =
    { loc.Location.loc_start with pos_cnum = loc.loc_start.pos_cnum + 3 }
  in
  read_string parent { loc with loc_start } str

let page parent loc str =
  let doc, _ = read_string parent loc str in
  `Docs doc

let standalone parent (attr : Parsetree.attribute) :
    Odoc_model.Comment.docs_or_stop option =
  match parse_attribute attr with
  | Some ("/*", _loc) -> Some `Stop
  | Some (str, loc) ->
      let doc, _ = read_string_comment parent loc str in
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

let rec extract_top_comment ~classify parent items =
  match items with
  | [] -> (items, empty, [])
  | hd :: tl -> (
      match classify hd with
      | Some (`Attribute attr) -> (
          match parse_attribute attr with
          | None -> (items, empty, [])
          | Some (str, loc) ->
              let doc, internal_tags =
                read_string_comment
                  (parent
                    : Paths.Identifier.Signature.t
                    :> Paths.Identifier.LabelParent.t)
                  loc str
              in
              (tl, doc, internal_tags))
      | Some `Open ->
          (* Skip opens *)
          let items, doc, tags = extract_top_comment ~classify parent tl in
          (hd :: items, doc, tags)
      | None -> (items, empty, []))

let extract_top_comment_class items =
  match items with
  | Lang.ClassSignature.Comment (`Docs doc) :: tl -> (tl, doc)
  | _ -> items, empty
