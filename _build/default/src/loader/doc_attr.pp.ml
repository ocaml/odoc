# 1 "src/loader/doc_attr.ml"
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

let point_of_pos { Lexing.pos_lnum; pos_bol; pos_cnum; _ } =
  let column = pos_cnum - pos_bol in
  { Odoc_model.Location_.line = pos_lnum; column }

let read_location { Location.loc_start; loc_end; _ } =
  {
    Odoc_model.Location_.file = loc_start.pos_fname;
    start = point_of_pos loc_start;
    end_ = point_of_pos loc_end;
  }

let empty_body = { Comment.elements = []; suppress_warnings = false }

let empty : Odoc_model.Comment.docs = empty_body

let load_constant_string = function
  | {Parsetree.pexp_desc =
     
# 43 "src/loader/doc_attr.ml"
     Pexp_constant (Pconst_string (text, _, _))
   
# 47 "src/loader/doc_attr.ml"
   ; pexp_loc = loc; _} ->
       Some (text , loc)
  | _ -> None

let load_payload = function
  | Parsetree.PStr [ { pstr_desc = Pstr_eval (constant_string, _); _ } ] ->
      load_constant_string constant_string
  | _ -> None

let load_alert_name name = (Longident.last name.Location.txt)

let load_alert_name_and_payload = function
  | Parsetree.PStr
      [ { pstr_desc = Pstr_eval ({ pexp_desc = expression; _ }, _); _ } ] -> (
      match expression with
      | Pexp_apply ({ pexp_desc = Pexp_ident name; _ }, [ (_, payload) ]) ->
          Some (load_alert_name name, load_constant_string payload)
      | Pexp_ident name -> Some (load_alert_name name, None)
      | _ -> None)
  | _ -> None

# 69 "src/loader/doc_attr.ml"
let attribute_unpack = function
  | { Parsetree.attr_name = { Location.txt = name; _ }; attr_payload; attr_loc } ->
      (name, attr_payload, attr_loc)

# 77 "src/loader/doc_attr.ml"
type payload = string * Location.t

type parsed_attribute =
  [ `Text of payload  (* Standalone comment. *)
  | `Doc of payload  (* Attached comment. *)
  | `Stop of Location.t  (* [(**/**)]. *)
  | `Alert of  string * payload option * Location.t
    (* [`Alert (name, payload, loc)] is for [\[@@alert name "payload"\]] attributes. *) ]

(** Recognize an attribute. *)
let parse_attribute : Parsetree.attribute -> parsed_attribute option =
 fun attr ->
  let name, attr_payload, attr_loc = attribute_unpack attr in
  match name with
  | "text" | "ocaml.text" -> (
      match load_payload attr_payload with
      | Some ("/*", _) -> Some (`Stop attr_loc)
      | Some p -> Some (`Text p)
      | None -> None)
  | "doc" | "ocaml.doc" -> (
      (* We don't expect a stop-comment here. *)
      match load_payload attr_payload with
      | Some p -> Some (`Doc p)
      | None -> None)
  | "deprecated" | "ocaml.deprecated" ->
      Some (`Alert ("deprecated", (load_payload attr_payload), attr_loc))
  | "alert" | "ocaml.alert" ->
      (match load_alert_name_and_payload attr_payload with
        Some (name, payload) ->
      Some (`Alert (name, payload, attr_loc))
      | None -> None)
  | _ -> None

let is_stop_comment attr =
  match parse_attribute attr with Some (`Stop _) -> true | _ -> false

let pad_loc loc =
  { loc.Location.loc_start with pos_cnum = loc.loc_start.pos_cnum + 3 }

let ast_to_comment ~internal_tags parent ast_docs alerts =
  Odoc_model.Semantics.ast_to_comment ~internal_tags
    ~tags_allowed:true ~parent_of_sections:parent ast_docs alerts
  |> Error.raise_warnings

let mk_alert_payload ~loc name p =
  let p = match p with Some (p, _) -> Some p | None -> None in
  let elt = `Tag (`Alert (name, p)) in
  let span = read_location loc in
  Location_.at span elt

let attached ~suppress_warnings internal_tags parent attrs =
  let rec loop acc_docs acc_alerts = function
    | attr :: rest -> (
        match parse_attribute attr with
        | Some (`Doc (str, loc)) ->
            let ast_docs =
              Odoc_parser.parse_comment ~location:(pad_loc loc) ~text:str
              |> Error.raise_parser_warnings
            in
            loop (List.rev_append ast_docs acc_docs) acc_alerts rest
        | Some (`Alert (name, p, loc)) ->
            let elt = mk_alert_payload ~loc name p in
            loop acc_docs (elt :: acc_alerts) rest
        | Some (`Text _ | `Stop _) | None -> loop acc_docs acc_alerts rest)
    | [] -> (List.rev acc_docs, List.rev acc_alerts)
  in
  let ast_docs, alerts = loop [] [] attrs in
  let elements, warnings = ast_to_comment ~internal_tags parent ast_docs alerts in
  { Comment.elements; suppress_warnings }, warnings

let attached_no_tag ~suppress_warnings parent attrs =
  let x, () = attached ~suppress_warnings Semantics.Expect_none parent attrs in
  x

let read_string ~tags_allowed internal_tags parent location str =
  Odoc_model.Semantics.parse_comment
    ~internal_tags
    ~tags_allowed
    ~containing_definition:parent
    ~location
    ~text:str
  |> Odoc_model.Error.raise_warnings

let read_string_comment internal_tags parent loc str =
  read_string ~tags_allowed:true internal_tags parent (pad_loc loc) str

let page parent loc str =
  let elements, tags = read_string ~tags_allowed:false Odoc_model.Semantics.Expect_page_tags parent loc.Location.loc_start
      str
  in
  { Comment.elements; suppress_warnings = false}, tags

let standalone parent ~suppress_warnings (attr : Parsetree.attribute) :
    Odoc_model.Comment.docs_or_stop option =
  match parse_attribute attr with
  | Some (`Stop _loc) -> Some `Stop
  | Some (`Text (str, loc)) ->
      let elements, () = read_string_comment Semantics.Expect_none parent loc str in
      Some (`Docs { elements; suppress_warnings })
  | Some (`Doc _) -> None
  | Some (`Alert (name, _, attr_loc)) ->
      let w =
        Error.make "Alert %s not expected here." name (read_location attr_loc)
      in
      Error.raise_warning w;
      None
  | _ -> None

let standalone_multiple parent ~suppress_warnings attrs =
  let coms =
    List.fold_left
      (fun acc attr ->
        match standalone parent ~suppress_warnings attr  with
         | None -> acc
         | Some com -> com :: acc)
      [] attrs
  in
    List.rev coms

let split_docs docs =
  let rec inner first x =
    match x with
    | { Location_.value = `Heading _; _ } :: _ -> List.rev first, x
    | x :: y -> inner (x::first) y
    | [] -> List.rev first, []
  in
  inner [] docs

let extract_top_comment internal_tags ~classify parent items =
  let classify x =
    match classify x with
    | Some (`Attribute attr) -> (
        match parse_attribute attr with
        | Some (`Text _ as p) -> p
        | Some (`Doc _) -> `Skip (* Unexpected, silently ignore *)
        | Some (`Alert (name, p, attr_loc)) ->
            let p = match p with Some (p, _) -> Some p | None -> None in
            let attr_loc = read_location attr_loc in
            `Alert (Location_.at attr_loc (`Tag (`Alert (name, p))))
        | Some (`Stop _) -> `Return (* Stop at stop-comments. *)
        | None -> `Skip (* Skip unrecognized attributes. *))
    | Some `Open -> `Skip (* Skip open statements *)
    | None -> `Return
  in
  let rec extract_tail_alerts acc = function
    (* Accumulate the alerts after the top-comment. Stop at the next comment. *)
    | hd :: tl as items -> (
        match classify hd with
        | `Text _ | `Return -> (items, acc)
        | `Alert alert -> extract_tail_alerts (alert :: acc) tl
        | `Skip -> extract_tail_alerts acc tl)
    | [] -> ([], acc)
  and extract = function
    (* Extract the first comment and accumulate the alerts before and after
       it. *)
    | hd :: tl as items -> (
        match classify hd with
        | `Text (text, loc) ->
            let ast_docs =
              Odoc_parser.parse_comment ~location:(pad_loc loc) ~text
              |> Error.raise_parser_warnings
            in
            let items, alerts = extract_tail_alerts [] tl in
            (items, ast_docs, alerts)
        | `Alert alert ->
            let items, ast_docs, alerts = extract tl in
            (items, ast_docs, alert :: alerts)
        | `Skip ->
            let items, ast_docs, alerts = extract tl in
            (hd :: items, ast_docs, alerts)
        | `Return -> (items, [], []))
    | [] -> ([], [], [])
  in
  let items, ast_docs, alerts = extract items in
  let docs, tags =
    ast_to_comment ~internal_tags
      (parent : Paths.Identifier.Signature.t :> Paths.Identifier.LabelParent.t)
      ast_docs alerts
  in
  let d1, d2 = split_docs docs in
  ( items,
    ( { Comment.elements = d1; suppress_warnings = false },
      { Comment.elements = d2; suppress_warnings = false } ),
    tags )

let extract_top_comment_class items =
  let mk elements suppress_warnings = { Comment.elements; suppress_warnings } in
  match items with
  | Lang.ClassSignature.Comment (`Docs doc) :: tl ->
      let d1, d2 = split_docs doc.elements in
      (tl, (mk d1 doc.suppress_warnings, mk d2 doc.suppress_warnings))
  | _ -> (items, (mk [] false, mk [] false))

let rec conv_canonical_module : Odoc_model.Reference.path -> Paths.Path.Module.t = function
  | `Dot (parent, name) -> `Dot (conv_canonical_module parent, Names.ModuleName.make_std name)
  | `Root name -> `Root (Names.ModuleName.make_std name)

let conv_canonical_type : Odoc_model.Reference.path -> Paths.Path.Type.t option = function
  | `Dot (parent, name) -> Some (`DotT (conv_canonical_module parent, Names.TypeName.make_std name))
  | _ -> None

let conv_canonical_module_type : Odoc_model.Reference.path -> Paths.Path.ModuleType.t option = function
  | `Dot (parent, name) -> Some (`DotMT (conv_canonical_module parent, Names.ModuleTypeName.make_std name))
  | _ -> None
