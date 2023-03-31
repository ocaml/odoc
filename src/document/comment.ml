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
    | `Hidden p -> render_resolved (p :> t)
    | `ModuleType (r, s) ->
        render_resolved (r :> t) ^ "." ^ ModuleTypeName.to_string s
    | `Type (r, s) -> render_resolved (r :> t) ^ "." ^ TypeName.to_string s
    | `Constructor (r, s) ->
        render_resolved (r :> t) ^ "." ^ ConstructorName.to_string s
    | `Field (r, s) -> render_resolved (r :> t) ^ "." ^ FieldName.to_string s
    | `Extension (r, s) ->
        render_resolved (r :> t) ^ "." ^ ExtensionName.to_string s
    | `ExtensionDecl (r, _, s) ->
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

  let rec render_unresolved : Reference.t -> string =
    let open Reference in
    function
    | `Resolved r -> render_resolved r
    | `Root (n, _) -> n
    | `Dot (p, f) -> render_unresolved (p :> t) ^ "." ^ f
    | `Module (p, f) ->
        render_unresolved (p :> t) ^ "." ^ ModuleName.to_string f
    | `ModuleType (p, f) ->
        render_unresolved (p :> t) ^ "." ^ ModuleTypeName.to_string f
    | `Type (p, f) -> render_unresolved (p :> t) ^ "." ^ TypeName.to_string f
    | `Constructor (p, f) ->
        render_unresolved (p :> t) ^ "." ^ ConstructorName.to_string f
    | `Field (p, f) -> render_unresolved (p :> t) ^ "." ^ FieldName.to_string f
    | `Extension (p, f) ->
        render_unresolved (p :> t) ^ "." ^ ExtensionName.to_string f
    | `ExtensionDecl (p, f) ->
        render_unresolved (p :> t) ^ "." ^ ExtensionName.to_string f
    | `Exception (p, f) ->
        render_unresolved (p :> t) ^ "." ^ ExceptionName.to_string f
    | `Value (p, f) -> render_unresolved (p :> t) ^ "." ^ ValueName.to_string f
    | `Class (p, f) -> render_unresolved (p :> t) ^ "." ^ ClassName.to_string f
    | `ClassType (p, f) ->
        render_unresolved (p :> t) ^ "." ^ ClassTypeName.to_string f
    | `Method (p, f) ->
        render_unresolved (p :> t) ^ "." ^ MethodName.to_string f
    | `InstanceVariable (p, f) ->
        render_unresolved (p :> t) ^ "." ^ InstanceVariableName.to_string f
    | `Label (p, f) -> render_unresolved (p :> t) ^ "." ^ LabelName.to_string f

  (* This is the entry point. *)
  let to_ir : ?text:Inline.t -> Reference.t -> Inline.t =
   fun ?text ref ->
    match ref with
    | `Resolved r -> (
        (* IDENTIFIER MUST BE RENAMED TO DEFINITION. *)
        let id = Reference.Resolved.identifier r in
        let rendered = render_resolved r in
        let content =
          match text with
          | None -> [ inline @@ Inline.Source (source_of_code rendered) ]
          | Some s -> s
        and tooltip =
          (* Add a tooltip if the content is not the rendered reference. *)
          match text with None -> None | Some _ -> Some rendered
        in
        match Url.from_identifier ~stop_before:false id with
        | Ok url ->
            let target = InternalLink.Resolved url in
            let link = { InternalLink.target; content; tooltip } in
            [ inline @@ Inline.InternalLink link ]
        | Error (Not_linkable _) -> content
        | Error exn ->
            (* FIXME: better error message *)
            Printf.eprintf "Id.href failed: %S\n%!" (Url.Error.to_string exn);
            content)
    | _ -> (
        let s = render_unresolved ref in
        match text with
        | None ->
            let s = source_of_code s in
            [ inline @@ Inline.Source s ]
        | Some content ->
            let link =
              { InternalLink.target = Unresolved; content; tooltip = Some s }
            in
            [ inline @@ Inline.InternalLink link ])
end

let leaf_inline_element : Comment.leaf_inline_element -> Inline.one = function
  | `Space -> inline @@ Text " "
  | `Word s -> inline @@ Text s
  | `Code_span s -> inline @@ Source (source_of_code s)
  | `Math_span s -> inline @@ Math s
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
      Reference.to_ir ?text:content path
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
      Reference.to_ir (m.module_reference :> Odoc_model.Paths.Reference.t)
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

let rec nestable_block_element :
    Comment.nestable_block_element -> Block.one list =
 fun content ->
  match content with
  | `Paragraph p -> [ paragraph p ]
  | `Code_block (lang_tag, code, outputs) ->
      let lang_tag =
        match lang_tag with None -> default_lang_tag | Some t -> t
      in
      let rest =
        match outputs with
        | Some xs -> nestable_block_element_list xs
        | None -> []
      in
      [
        block
        @@ Source (lang_tag, source_of_code (Odoc_model.Location_.value code));
      ]
      @ rest
  | `Math_block s -> [ block @@ Math s ]
  | `Verbatim s -> [ block @@ Verbatim s ]
  | `Modules ms -> [ module_references ms ]
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
      [ block @@ Block.List (kind, items) ]
  | `Table { data; align } ->
      let data =
        List.map
          (List.map (fun (cell, cell_type) ->
               (nestable_block_element_list cell, cell_type)))
          data
      in
      let generate_align data =
        let max (a : int) b = if a < b then b else a in
        (* Length of the longest line of the table *)
        let max_length =
          List.fold_left (fun m l -> max m (List.length l)) 0 data
        in
        let rec list_init i =
          if i <= 0 then [] else Table.Default :: list_init (i - 1)
        in
        list_init max_length
      in
      let align =
        match align with
        | None -> generate_align data
        | Some align ->
            List.map
              (function
                | None -> Table.Default
                | Some `Right -> Right
                | Some `Left -> Left
                | Some `Center -> Center)
              align
        (* We should also check wellness of number of table cells vs alignment,
           and raise warnings *)
      in
      [ block @@ Table { data; align } ]

and paragraph : Comment.paragraph -> Block.one = function
  | [ { value = `Raw_markup (target, s); _ } ] ->
      block @@ Block.Raw_markup (target, s)
  | p -> block @@ Block.Paragraph (inline_element_list p)

and nestable_block_element_list :
    Comment.nestable_block_element Comment.with_location list -> Block.one list
    =
 fun elements ->
  elements
  |> List.map Odoc_model.Location_.value
  |> List.map nestable_block_element
  |> List.concat

let tag : Comment.tag -> Description.one =
 fun t ->
  let sp = inline (Text " ") in
  let item ?value ~tag definition =
    let tag_name = inline ~attr:[ "at-tag" ] (Text tag) in
    let tag_value = match value with None -> [] | Some t -> sp :: t in
    let key = tag_name :: tag_value in
    { Description.attr = [ tag ]; key; definition }
  in
  let mk_value desc = [ inline ~attr:[ "value" ] desc ] in
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
      let value = mk_value (Inline.Text name) in
      item ~tag:"parameter" ~value (nestable_block_element_list content)
  | `Raise (kind, content) ->
      let value = inline_element (kind :> Comment.inline_element) in
      item ~tag:"raises" ~value (nestable_block_element_list content)
  | `Return content -> item ~tag:"returns" (nestable_block_element_list content)
  | `See (kind, target, content) ->
      let value =
        match kind with
        | `Url -> mk_value (Inline.Link (target, [ inline @@ Text target ]))
        | `File -> mk_value (Inline.Source (source_of_code target))
        | `Document -> mk_value (Inline.Text target)
      in
      item ~tag:"see" ~value (nestable_block_element_list content)
  | `Since s -> item ~tag:"since" (text_def s)
  | `Before (version, content) ->
      let value = mk_value (Inline.Text version) in
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
  | #Comment.nestable_block_element as e -> nestable_block_element e
  | `Tag t -> [ block ~attr:[ "at-tags" ] @@ Description [ tag t ] ]

(* TODO collaesce tags *)

let block_element : Comment.block_element -> Block.t = function
  | #Comment.attached_block_element as e -> attached_block_element e
  | `Heading (_, _, text) ->
      (* We are not supposed to receive Heading in this context.
         TODO: Remove heading in attached documentation in the model *)
      [ block @@ Paragraph (inline_element_list text) ]

let heading_level_to_int = function
  | `Title -> 0
  | `Section -> 1
  | `Subsection -> 2
  | `Subsubsection -> 3
  | `Paragraph -> 4
  | `Subparagraph -> 5

let heading
    (attrs, { Odoc_model.Paths.Identifier.iv = `Label (_, label); _ }, text) =
  let label = Odoc_model.Names.LabelName.to_string label in
  let title = inline_element_list text in
  let level = heading_level_to_int attrs.Comment.heading_level in
  let label = Some label in
  let source_anchor = None in
  Item.Heading { label; level; title; source_anchor }

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
