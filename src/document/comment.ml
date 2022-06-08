(*
 * Copyright (c) 2016, 2017 Thomas Refis <trefis@janestreet.com>
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

open Types
module Comment = Odoc_model.Comment
open Odoc_model.Names

let default_lang_tag = "ocaml"

let source_of_code s =
  if s = "" then [] else [ Source.Elt [ inline @@ Inline.Text s ] ]

module Reference = struct
  open Odoc_model.Paths

  let rec render_resolved : Reference.Resolved.t -> string =
   fun r ->
    let open Reference.Resolved in
    match r with
    | `Identifier id -> Identifier.name id
    | `Alias (_, r) -> render_resolved (r :> t)
    | `AliasModuleType (_, r) -> render_resolved (r :> t)
    | `Module (r, s) -> render_resolved (r :> t) ^ "." ^ ModuleName.to_string s
    | `Canonical (_, `Resolved r) -> render_resolved (r :> t)
    | `Canonical (p, _) -> render_resolved (p :> t)
    | `Hidden p -> render_resolved (p :> t)
    | `ModuleType (r, s) ->
        render_resolved (r :> t) ^ "." ^ ModuleTypeName.to_string s
    | `Type (r, s) -> render_resolved (r :> t) ^ "." ^ TypeName.to_string s
    | `Constructor (r, s) ->
        render_resolved (r :> t) ^ "." ^ ConstructorName.to_string s
    | `Field (r, s) -> render_resolved (r :> t) ^ "." ^ FieldName.to_string s
    | `Extension (r, s) ->
        render_resolved (r :> t) ^ "." ^ ExtensionName.to_string s
    | `Exception (r, s) ->
        render_resolved (r :> t) ^ "." ^ ExceptionName.to_string s
    | `Value (r, s) -> render_resolved (r :> t) ^ "." ^ ValueName.to_string s
    | `Class (r, s) -> render_resolved (r :> t) ^ "." ^ ClassName.to_string s
    | `ClassType (r, s) ->
        render_resolved (r :> t) ^ "." ^ ClassTypeName.to_string s
    | `Method (r, s) ->
        (* CR trefis: do we really want to print anything more than [s] here?  *)
        render_resolved (r :> t) ^ "." ^ MethodName.to_string s
    | `InstanceVariable (r, s) ->
        (* CR trefis: the following makes no sense to me... *)
        render_resolved (r :> t) ^ "." ^ InstanceVariableName.to_string s
    | `Label (_, s) -> LabelName.to_string s

  (* This is the entry point. stop_before is false on entry, true on recursive
     call. *)
  let rec to_ir : ?text:Inline.t -> stop_before:bool -> Reference.t -> Inline.t
      =
   fun ?text ~stop_before ref ->
    let open Reference in
    match ref with
    | `Root (s, _) -> (
        match text with
        | None ->
            let s = source_of_code s in
            [ inline @@ Inline.Source s ]
        | Some s ->
            [ inline @@ Inline.InternalLink (InternalLink.Unresolved s) ])
    | `Dot (parent, s) -> unresolved ?text (parent :> t) s
    | `Module (parent, s) ->
        unresolved ?text (parent :> t) (ModuleName.to_string s)
    | `ModuleType (parent, s) ->
        unresolved ?text (parent :> t) (ModuleTypeName.to_string s)
    | `Type (parent, s) -> unresolved ?text (parent :> t) (TypeName.to_string s)
    | `Constructor (parent, s) ->
        unresolved ?text (parent :> t) (ConstructorName.to_string s)
    | `Field (parent, s) ->
        unresolved ?text (parent :> t) (FieldName.to_string s)
    | `Extension (parent, s) ->
        unresolved ?text (parent :> t) (ExtensionName.to_string s)
    | `Exception (parent, s) ->
        unresolved ?text (parent :> t) (ExceptionName.to_string s)
    | `Value (parent, s) ->
        unresolved ?text (parent :> t) (ValueName.to_string s)
    | `Class (parent, s) ->
        unresolved ?text (parent :> t) (ClassName.to_string s)
    | `ClassType (parent, s) ->
        unresolved ?text (parent :> t) (ClassTypeName.to_string s)
    | `Method (parent, s) ->
        unresolved ?text (parent :> t) (MethodName.to_string s)
    | `InstanceVariable (parent, s) ->
        unresolved ?text (parent :> t) (InstanceVariableName.to_string s)
    | `Label (parent, s) ->
        unresolved ?text (parent :> t) (LabelName.to_string s)
    | `Resolved r -> (
        (* IDENTIFIER MUST BE RENAMED TO DEFINITION. *)
        let id = Reference.Resolved.identifier r in
        let txt =
          match text with
          | None ->
              [ inline @@ Inline.Source (source_of_code (render_resolved r)) ]
          | Some s -> s
        in
        match Url.from_identifier ~stop_before id with
        | Ok url ->
            [ inline @@ Inline.InternalLink (InternalLink.Resolved (url, txt)) ]
        | Error (Not_linkable _) -> txt
        | Error exn ->
            (* FIXME: better error message *)
            Printf.eprintf "Id.href failed: %S\n%!" (Url.Error.to_string exn);
            txt)

  and unresolved : ?text:Inline.t -> Reference.t -> string -> Inline.t =
   fun ?text parent field ->
    match text with
    | Some s -> [ inline @@ InternalLink (InternalLink.Unresolved s) ]
    | None ->
        let tail = [ inline @@ Text ("." ^ field) ] in
        let content = to_ir ~stop_before:true parent in
        content @ tail
end

let leaf_inline_element : Comment.leaf_inline_element -> Inline.one = function
  | `Space -> inline @@ Text " "
  | `Word s -> inline @@ Text s
  | `Code_span s -> inline @@ Source (source_of_code s)
  | `Raw_markup (target, s) -> inline @@ Raw_markup (target, s)

let rec non_link_inline_element : Comment.non_link_inline_element -> Inline.one
    = function
  | #Comment.leaf_inline_element as e -> leaf_inline_element e
  | `Styled (style, content) ->
      inline @@ Styled (style, non_link_inline_element_list content)

and non_link_inline_element_list : _ -> Inline.t =
 fun elements ->
  List.map
    (fun elt -> non_link_inline_element elt.Odoc_model.Location_.value)
    elements

let link_content = non_link_inline_element_list

let rec inline_element : Comment.inline_element -> Inline.t = function
  | #Comment.leaf_inline_element as e -> [ leaf_inline_element e ]
  | `Styled (style, content) ->
      [ inline @@ Styled (style, inline_element_list content) ]
  | `Reference (path, content) ->
      (* TODO Rework that ugly function. *)
      (* TODO References should be set in code style, if they are to code
              elements. *)
      let content =
        match content with
        | [] -> None
        | _ -> Some (non_link_inline_element_list content)
        (* XXX Span *)
      in
      Reference.to_ir ?text:content ~stop_before:false path
  | `Link (target, content) ->
      let content =
        match content with
        | [] -> [ inline @@ Text target ]
        | _ -> non_link_inline_element_list content
      in
      [ inline @@ Link (target, content) ]

and inline_element_list elements =
  List.concat
  @@ List.map
       (fun elt -> inline_element elt.Odoc_model.Location_.value)
       elements

let module_references ms =
  let module_reference (m : Comment.module_reference) =
    let reference =
      Reference.to_ir ~stop_before:false
        (m.module_reference :> Odoc_model.Paths.Reference.t)
    and synopsis =
      match m.module_synopsis with
      | Some synopsis ->
          [
            block ~attr:[ "synopsis" ] @@ Inline (inline_element_list synopsis);
          ]
      | None -> []
    in
    { Description.attr = []; key = reference; definition = synopsis }
  in
  let items = List.map module_reference ms in
  block ~attr:[ "modules" ] @@ Description items

let rec nestable_block_element : Comment.nestable_block_element -> Block.one =
 fun content ->
  match content with
  | `Paragraph p -> paragraph p
  | `Code_block (lang_tag, code) ->
      let lang_tag =
        match lang_tag with None -> default_lang_tag | Some t -> t
      in
      block
      @@ Source (lang_tag, source_of_code (Odoc_model.Location_.value code))
  | `Verbatim s -> block @@ Verbatim s
  | `Modules ms -> module_references ms
  | `List (kind, items) ->
      let kind =
        match kind with
        | `Unordered -> Block.Unordered
        | `Ordered -> Block.Ordered
      in
      let f = function
        | [ { Odoc_model.Location_.value = `Paragraph content; _ } ] ->
            [ block @@ Block.Inline (inline_element_list content) ]
        | item -> nestable_block_element_list item
      in
      let items = List.map f items in
      block @@ Block.List (kind, items)

and paragraph : Comment.paragraph -> Block.one = function
  | [ { value = `Raw_markup (target, s); _ } ] ->
      block @@ Block.Raw_markup (target, s)
  | p -> block @@ Block.Paragraph (inline_element_list p)

and nestable_block_element_list elements =
  elements
  |> List.map Odoc_model.Location_.value
  |> List.map nestable_block_element

let tag : Comment.tag -> Description.one =
 fun t ->
  let sp = inline (Text " ") in
  let item ?value ~tag definition =
    let tag_name = inline ~attr:[ "at-tag" ] (Text tag) in
    let tag_value =
      match value with
      | None -> []
      | Some t -> [ sp; inline ~attr:[ "value" ] t ]
    in
    let key = tag_name :: tag_value in
    { Description.attr = [ tag ]; key; definition }
  in
  let text_def s = [ block (Block.Inline [ inline @@ Text s ]) ] in
  let content_to_inline ?(prefix = []) content =
    match content with
    | None -> []
    | Some content -> prefix @ [ inline @@ Text content ]
  in
  match t with
  | `Author s -> item ~tag:"author" (text_def s)
  | `Deprecated content ->
      item ~tag:"deprecated" (nestable_block_element_list content)
  | `Param (name, content) ->
      let value = Inline.Text name in
      item ~tag:"parameter" ~value (nestable_block_element_list content)
  | `Raise (name, content) ->
      let value = Inline.Text name in
      item ~tag:"raises" ~value (nestable_block_element_list content)
  | `Return content -> item ~tag:"returns" (nestable_block_element_list content)
  | `See (kind, target, content) ->
      let value =
        match kind with
        | `Url -> Inline.Link (target, [ inline @@ Text target ])
        | `File -> Inline.Source (source_of_code target)
        | `Document -> Inline.Text target
      in
      item ~tag:"see" ~value (nestable_block_element_list content)
  | `Since s -> item ~tag:"since" (text_def s)
  | `Before (version, content) ->
      let value = Inline.Text version in
      item ~tag:"before" ~value (nestable_block_element_list content)
  | `Version s -> item ~tag:"version" (text_def s)
  | `Alert ("deprecated", content) ->
      let content = content_to_inline content in
      item ~tag:"deprecated" [ block (Block.Inline content) ]
  | `Alert (tag, content) ->
      let content = content_to_inline ~prefix:[ sp ] content in
      item ~tag:"alert"
        [ block (Block.Inline ([ inline @@ Text tag ] @ content)) ]

let attached_block_element : Comment.attached_block_element -> Block.t =
  function
  | #Comment.nestable_block_element as e -> [ nestable_block_element e ]
  | `Tag t -> [ block ~attr:[ "at-tags" ] @@ Description [ tag t ] ]

(* TODO collaesce tags *)

let block_element : Comment.block_element -> Block.t = function
  | #Comment.attached_block_element as e -> attached_block_element e
  | `Heading (_, _, text) ->
      (* We are not supposed to receive Heading in this context.
         TODO: Remove heading in attached documentation in the model *)
      [ block @@ Paragraph (non_link_inline_element_list text) ]

let heading_level_to_int = function
  | `Title -> 0
  | `Section -> 1
  | `Subsection -> 2
  | `Subsubsection -> 3
  | `Paragraph -> 4
  | `Subparagraph -> 5

let heading (attrs, `Label (_, label), text) =
  let label = Odoc_model.Names.LabelName.to_string label in
  let title = non_link_inline_element_list text in
  let level = heading_level_to_int attrs.Comment.heading_level in
  let label = Some label in
  Item.Heading { label; level; title }

let item_element : Comment.block_element -> Item.t list = function
  | #Comment.attached_block_element as e ->
      [ Item.Text (attached_block_element e) ]
  | `Heading h -> [ heading h ]

(** The documentation of the expansion is used if there is no comment attached
    to the declaration. *)
let synopsis ~decl_doc ~expansion_doc =
  let ([], Some docs | docs, _) = (decl_doc, expansion_doc) in
  match Comment.synopsis docs with Some p -> [ paragraph p ] | None -> []

let standalone docs =
  Utils.flatmap ~f:item_element
  @@ List.map (fun x -> x.Odoc_model.Location_.value) docs

let to_ir (docs : Comment.docs) =
  Utils.flatmap ~f:block_element
  @@ List.map (fun x -> x.Odoc_model.Location_.value) docs

let has_doc docs = docs <> []
