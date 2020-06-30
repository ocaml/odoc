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

let source_of_code s =
  if s = "" then [] else [Source.Elt [inline @@ Inline.Text s]]

module Reference = struct
  open Odoc_model.Paths

  let rec render_resolved : Reference.Resolved.t -> string =
    fun r ->
      let open Reference.Resolved in
      match r with
      | `Identifier id -> Identifier.name id
      | `SubstAlias(_, r) -> render_resolved (r :> t)
      | `Module (r, s) -> render_resolved (r :> t) ^ "." ^ (ModuleName.to_string s)
      | `Canonical (_, `Resolved r) -> render_resolved (r :> t)
      | `Canonical (p, _) -> render_resolved (p :> t)
      | `Hidden p -> render_resolved (p :> t)
      | `ModuleType (r, s) -> render_resolved (r :> t) ^ "." ^ (ModuleTypeName.to_string s)
      | `Type (r, s) -> render_resolved (r :> t) ^ "." ^ (TypeName.to_string s)
      | `Constructor (r, s) -> render_resolved (r :> t) ^ "." ^ (ConstructorName.to_string s)
      | `Field (r, s) -> render_resolved (r :> t) ^ "." ^ (FieldName.to_string s)
      | `Extension (r, s) -> render_resolved (r :> t) ^ "." ^ (ExtensionName.to_string s)
      | `Exception (r, s) -> render_resolved (r :> t) ^ "." ^ (ExceptionName.to_string s)
      | `Value (r, s) -> render_resolved (r :> t) ^ "." ^ (ValueName.to_string s)
      | `Class (r, s) -> render_resolved (r :> t) ^ "." ^ (ClassName.to_string s)
      | `ClassType (r, s) -> render_resolved (r :> t) ^ "." ^ (ClassTypeName.to_string s)
      | `Method (r, s) ->
        (* CR trefis: do we really want to print anything more than [s] here?  *)
        render_resolved (r :> t) ^ "." ^ (MethodName.to_string s)
      | `InstanceVariable (r, s) ->
        (* CR trefis: the following makes no sense to me... *)
        render_resolved (r :> t) ^ "." ^ (InstanceVariableName.to_string s)
      | `Label (r, s) -> render_resolved (r :> t) ^ ":" ^ (LabelName.to_string s)



  let rec ref_to_string : Reference.t -> string =
    let open Reference in
    function
    | `Root (s, _) -> s
    | `Dot (parent, s) -> ref_to_string (parent :> t) ^ "." ^ s
    | `Module (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ModuleName.to_string s)
    | `ModuleType (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ModuleTypeName.to_string s)
    | `Type (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (TypeName.to_string s)
    | `Constructor (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ConstructorName.to_string s)
    | `Field (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (FieldName.to_string s)
    | `Extension (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ExtensionName.to_string s)
    | `Exception (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ExceptionName.to_string s)
    | `Value (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ValueName.to_string s)
    | `Class (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ClassName.to_string s)
    | `ClassType (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (ClassTypeName.to_string s)
    | `Method (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (MethodName.to_string s)
    | `InstanceVariable (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (InstanceVariableName.to_string s)
    | `Label (parent, s) -> ref_to_string (parent :> t) ^ "." ^ (LabelName.to_string s)
    | `Resolved r -> render_resolved r


  (* This is the entry point. stop_before is false on entry, true on recursive
     call. *)
  let rec to_ir
    : ?text:Inline.t ->
      stop_before:bool ->
      Reference.t ->
      Inline.t =
    fun ?text ~stop_before ref ->
    let open Reference in
    match ref with
    | `Root (s, _) ->
      begin match text with
        | None ->
          let s = source_of_code s in
          [inline @@ Inline.Source s]
        | Some s ->
          [inline @@ Inline.InternalLink (InternalLink.Unresolved s)]
      end
    | `Dot (parent, s) ->
      unresolved ?text (parent :> t) s
    | `Module (parent, s) ->
      unresolved ?text (parent :> t) (ModuleName.to_string s)
    | `ModuleType (parent, s) ->
      unresolved ?text (parent :> t) (ModuleTypeName.to_string s)
    | `Type (parent, s) ->
      unresolved ?text (parent :> t) (TypeName.to_string s)
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
    | `Resolved r ->
      (* IDENTIFIER MUST BE RENAMED TO DEFINITION. *)
      let id = Reference.Resolved.identifier r in
      let txt =
        match text with
        | None -> [inline @@ Inline.Source (source_of_code (render_resolved r))]
        | Some s -> s
      in
      begin match Url.from_identifier ~stop_before id with
      | Ok url ->
        [inline @@ Inline.InternalLink (InternalLink.Resolved (url, txt))]
      | Error (Not_linkable _) -> txt
      | Error exn ->
        (* FIXME: better error message *)
        Printf.eprintf "Id.href failed: %S\n%!" (Url.Error.to_string exn);
        txt
      end

  and unresolved
    : ?text:Inline.t ->
      Reference.t ->
      string -> Inline.t =
    fun ?text parent field ->
    match text with
    | Some s ->
      [inline @@ InternalLink (InternalLink.Unresolved s)]
    | None ->
      let tail = [inline @@ Text ("." ^ field) ] in
      let content = to_ir ~stop_before:true parent in
      content @ tail
end


let leaf_inline_element
    : Comment.leaf_inline_element -> Inline.one =
  function
  | `Space -> inline @@ Text " "
  | `Word s -> inline @@ Text s
  | `Code_span s -> inline @@ Source (source_of_code s)
  | `Raw_markup (target, s) -> inline @@ Raw_markup (target, s)

let rec non_link_inline_element
    : Comment.non_link_inline_element -> Inline.one =
  function
  | #Comment.leaf_inline_element as e -> leaf_inline_element e
  | `Styled (style, content) ->
    inline @@ Styled (style, non_link_inline_element_list content)

and non_link_inline_element_list : _ -> Inline.t = fun elements ->
  List.map
    (fun elt -> non_link_inline_element elt.Odoc_model.Location_.value)
    elements

let link_content =
  non_link_inline_element_list



let rec inline_element : Comment.inline_element -> Inline.t =
  function
  | #Comment.leaf_inline_element as e -> [leaf_inline_element e]
  | `Styled (style, content) ->
    [inline @@ Styled (style, inline_element_list content)]
  | `Reference (path, content) ->
    (* TODO Rework that ugly function. *)
    (* TODO References should be set in code style, if they are to code
            elements. *)
    let content =
      match content with
      | [] -> None
      | _ -> Some (non_link_inline_element_list content) (* XXX Span *)
    in
    Reference.to_ir ?text:content ~stop_before:false path
  | `Link (target, content) ->
    let content =
      match content with
      | [] -> [inline @@ Text target]
      | _ -> non_link_inline_element_list content
    in
    [inline @@ Link (target, content)]

and inline_element_list elements =
  List.concat @@ List.map
    (fun elt -> inline_element elt.Odoc_model.Location_.value)
    elements


let rec nestable_block_element
  : Comment.nestable_block_element -> Block.one =
  fun content ->
  match content with
  | `Paragraph [{value = `Raw_markup (target, s); _}] ->
    block @@ Block.Raw_markup (target, s)
  | `Paragraph content ->
    block @@ Block.Paragraph (inline_element_list content)
  | `Code_block code ->
    block @@ Source (source_of_code code)
  | `Verbatim s ->
    block @@ Verbatim s
  | `Modules ms ->
    let items =
      List.map
        (fun r ->
            [block @@ Inline (Reference.to_ir ~stop_before:false r)])
        (ms :> Odoc_model.Paths.Reference.t list)
    in
    block ~attr:["modules"] @@ Block.List (Unordered, items)
  | `List (kind, items) ->
    let kind = match kind with
      | `Unordered -> Block.Unordered
      | `Ordered -> Block.Ordered
    in
    let f = function
      | [{Odoc_model.Location_.value = `Paragraph content; _}] ->
        [block @@ Block.Inline (inline_element_list content)]
      | item ->
        nestable_block_element_list item
    in
    let items = List.map f items in
    block @@ Block.List (kind, items)

and nestable_block_element_list elements =
  elements
  |> List.map Odoc_model.Location_.value
  |> List.map nestable_block_element

let tag : Comment.tag -> Block.t = fun t ->
  let description a b = [block @@ Description [ a, b ]] in
  match t with
  | `Author s ->
    description
      [inline @@ Text "author"]
      [{ attr = [] ; desc = Block.Inline [inline @@ Text s] }]
  | `Deprecated content ->
    description
      [inline @@ Text "deprecated"]
      (nestable_block_element_list content)
  | `Param (name, content) ->
    description
      [inline @@ Text "parameter "; inline @@ Text name]
      (nestable_block_element_list content)
  | `Raise (name, content) ->
    description
      [inline @@ Text "raises "; inline @@ Text name]
      (nestable_block_element_list content)
  | `Return content ->
    description
      [inline @@ Text "returns"]
      (nestable_block_element_list content)
  | `See (kind, target, content) ->
    let target =
      match kind with
      | `Url -> Inline.Link (target, [inline @@ Text target])
      | `File -> Inline.Source (source_of_code target)
      | `Document -> Inline.Text target
    in
    description
      [inline @@ Text "see "; inline @@ target]
      (nestable_block_element_list content)
  | `Since s ->
    description
      [inline @@ Text "since"]
      [{ attr = []; desc = Block.Inline [inline @@ Text s]}]
  | `Before (version, content) ->
    description
      [inline @@ Text "before "; inline @@ Text version]
      (nestable_block_element_list content)
  | `Version s ->
    description
      [inline @@ Text "version"]
      [{ attr = []; desc = Block.Inline [inline @@ Text s]}]
  | `Canonical _ | `Inline | `Open | `Closed ->
    []

let attached_block_element : Comment.attached_block_element -> Block.t = function
  | #Comment.nestable_block_element as e ->
    [nestable_block_element e]
  | `Tag t ->
    tag t

let block_element : Comment.block_element -> Block.t = function
  | #Comment.attached_block_element as e ->
    attached_block_element e
  | `Heading (_, `Label (_, _), content) ->
    (* We are not supposed to receive Heading in this context.
       TODO: Remove heading in attached documentation in the model *)
    [block @@ Paragraph (non_link_inline_element_list content)]

let heading (`Heading (level, `Label (_, label), content)) =
    let label = Odoc_model.Names.LabelName.to_string label in
    let title = non_link_inline_element_list content in
    let level =
      match level with
      | `Title -> 0
      | `Section -> 1
      | `Subsection -> 2
      | `Subsubsection -> 3
      | `Paragraph -> 4
      | `Subparagraph -> 5
    in
    let label = Some label in
    Item.Heading {label; level; title}

let item_element : Comment.block_element -> Item.t list = function
  | #Comment.attached_block_element as e ->
    [Item.Text (attached_block_element e)]
  | `Heading _ as h ->
    [heading h]

let first_to_ir = function
  | {Odoc_model.Location_.value = `Paragraph _ as first_paragraph ; _} ::_
    ->
    block_element first_paragraph
  | _ -> []

let standalone docs =
  Utils.flatmap ~f:item_element @@
  List.map (fun x -> x.Odoc_model.Location_.value) docs

let to_ir (docs : Comment.docs) =
  Utils.flatmap ~f:block_element @@
  List.map (fun x -> x.Odoc_model.Location_.value) docs

let has_doc docs =
  docs <> []
