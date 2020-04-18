(*
 * Copyright (c) 2016 Thomas Refis <trefis@janestreet.com>
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

open Odoc_document.Types

module Html = Tyxml.Html
module Doctree = Odoc_document.Doctree

type any = Html_types.flow5
type item = Html_types.flow5_without_header_footer
type flow = Html_types.flow5_without_sectioning_heading_header_footer
type phrasing = Html_types.phrasing
type non_link_phrasing = Html_types.phrasing_without_interactive

let mk_anchor_link id =
  [ Html.a ~a:[Html.a_href ("#" ^ id); Html.a_class ["anchor"]] [] ]
let mk_anchor anchor = match anchor with
  | None -> [], []
  | Some {Odoc_document.Url.Anchor. anchor ; _ } ->
    let link = mk_anchor_link anchor in
    let attrib = [ Html.a_id anchor; Html.a_class ["anchored"] ] in
    attrib, link

let class_ (l : Class.t) =
  if l = [] then [] else [Html.a_class l]

and raw_markup (t : Raw_markup.t) =
  let target, content = t in
  if `Html = target then
    [Html.Unsafe.data content] (* This is wrong *)
  else
    []

and source k ?a (t : Source.t) =
  let rec token (x : Source.token) = match x with
    | Elt i -> k i
    | Tag (None, l) -> [Html.span (tokens l)]
    | Tag (Some s, l) -> [Html.span ~a:[Html.a_class [s]] (tokens l)]
  and tokens t = Utils.list_concat_map t ~f:token
  in
  Utils.optional_elt Html.code ?a (tokens t)

and styled style ~emph_level = match style with
  | `Emphasis ->
    let a = if emph_level mod 2 = 0 then [] else [Html.a_class ["odd"]] in
    emph_level+1, Html.em ~a
  | `Bold -> emph_level, Html.b ~a:[]
  | `Italic -> emph_level, Html.i ~a:[]
  | `Superscript -> emph_level, Html.sup ~a:[]
  | `Subscript -> emph_level, Html.sub ~a:[]

let rec internallink
    ~emph_level ~resolve
    ?(a=[])
    (t : InternalLink.t) = match t with
  | Resolved (uri, content) ->
    let href = Link.href ~resolve uri in
    let a = (a :> Html_types.a_attrib Html.attrib list) in
    let elt = Html.a ~a:(Html.a_href href :: a)
      (inline_nolink ~emph_level content) in
    let elt = (elt :> phrasing Html.elt) in
    [elt]
  | Unresolved content ->
    (* let title =
     *   Html.a_title (Printf.sprintf "unresolved reference to %S"
     *       (ref_to_string ref)
     * in *)
    let a = Html.a_class ["xref-unresolved"] :: a in
    let elt = Html.span ~a (inline ~emph_level ~resolve content) in
    let elt = (elt :> phrasing Html.elt) in
    [elt]

and internallink_nolink
    ~emph_level
    ~(a: Html_types.span_attrib Html.attrib list)
    (t : InternalLink.t) = match t with
  | Resolved (_, content)
  | Unresolved content ->
    [Html.span ~a (inline_nolink ~emph_level content)]

and inline
    ?(emph_level=0) ~resolve (l : Inline.t) : phrasing Html.elt list =
  let one (t : Inline.one) =
    let a = class_ t.attr in
    match t.desc with
    | Text s ->
      if a = [] then [Html.txt s] else [Html.span ~a [Html.txt s]]
    | Entity s ->
      if a = [] then [Html.entity s] else [Html.span ~a [Html.entity s]]
    | Linebreak ->
      [Html.br ~a ()]
    | Styled (style, c) ->
      let emph_level, app_style = styled style ~emph_level in
      [app_style @@ inline ~emph_level ~resolve c]
    | Link (href, c) ->
      let a = (a :> Html_types.a_attrib Html.attrib list) in
      let content = inline_nolink ~emph_level c in
      [Html.a ~a:(Html.a_href href :: a) content]
    | InternalLink c ->
      internallink ~emph_level ~resolve ~a c
    | Source c ->
      source (inline ~emph_level ~resolve) ~a c
    | Raw_markup r ->
      raw_markup r
  in
  Utils.list_concat_map ~f:one l

and inline_nolink
    ?(emph_level=0) (l : Inline.t) : non_link_phrasing Html.elt list =
  let one (t : Inline.one) =
    let a = class_ t.attr in
    match t.desc with
    | Text s ->
      if a = [] then [Html.txt s] else [Html.span ~a [Html.txt s]]
    | Entity s ->
      if a = [] then [Html.entity s] else [Html.span ~a [Html.entity s]]
    | Linebreak ->
      [Html.br ~a ()]
    | Styled (style, c) ->
      let emph_level, app_style = styled style ~emph_level in
      [ app_style @@ inline_nolink ~emph_level c]
    | Link (_, c) ->
      inline_nolink ~emph_level c
    | InternalLink c ->
      internallink_nolink ~emph_level ~a c
    | Source c ->
      source (inline_nolink ~emph_level) ~a c
    | Raw_markup r ->
      raw_markup r
  in
  Utils.list_concat_map ~f:one l

let heading ~resolve (h : Heading.t) =
  let a, anchor = match h.label with
    | Some id -> [Html.a_id id], mk_anchor_link id
    | None -> [], []
  in
  let content = inline ~resolve h.title in
  let mk =
    match h.level with
    | 1 -> Html.h1
    | 2 -> Html.h2
    | 3 -> Html.h3
    | 4 -> Html.h4
    | 5 -> Html.h5
    | _ -> Html.h6
  in
  mk ~a (anchor @ content)

let rec block ~resolve (l: Block.t) : flow Html.elt list =
  let as_flow x =
    (x : phrasing Html.elt list :> flow Html.elt list)
  in
  let one (t : Block.one) =
    let a = class_ t.attr in
    match t.desc with
    | Inline i ->
      if a = [] then
        as_flow @@ inline ~resolve i
      else
        [Html.span ~a (inline ~resolve i)]
    | Paragraph i ->
      [Html.p ~a (inline ~resolve i)]
    | List (typ, l) ->
      let mk = match typ with Ordered -> Html.ol | Unordered -> Html.ul in
      [mk ~a (List.map (fun x -> Html.li (block ~resolve x)) l)]
    | Description l ->
      [Html.dl ~a (Utils.list_concat_map l ~f:(fun (i,b) ->
          let i =
            (inline ~resolve i
             : phrasing Html.elt list
              :> flow Html.elt list)
          in
          [Html.dt i ; Html.dd (block ~resolve b) ]
        ))]
    | Raw_markup r ->
      raw_markup r
    | Verbatim s ->
      [Html.pre ~a [Html.txt s]]
    | Source c ->
      [Html.pre ~a (source (inline ~resolve) c)]
  in
  Utils.list_concat_map l ~f:one


let documentedSrc ~resolve (t : DocumentedSrc.t) =
  let open DocumentedSrc in
  let take_code l =
    Doctree.Take.until l ~classify:(function
      | Code code -> Accum code
      | _ -> Stop_and_keep
    )
  in
  let take_descr l =
    Doctree.Take.until l ~classify:(function
      | Documented { attrs; anchor; code; doc }  ->
        Accum [{DocumentedSrc. attrs ; anchor ; code = `D code; doc }]
      | Nested { attrs; anchor; code; doc } ->
        Accum [{DocumentedSrc. attrs ; anchor ; code = `N code; doc }]
      | _ -> Stop_and_keep
    )
  in
  let rec to_html t : flow Html.elt list = match t with
    | [] -> []
    | Code _ :: _ ->
      let code, _, rest = take_code t in
      source (inline ~resolve) code
      @ to_html rest
    | (Documented _ | Nested _) :: _ ->
      let l, _, rest = take_descr t in
      let one {DocumentedSrc. attrs ; anchor ; code ; doc } =
        let content = match code with
          | `D code -> (inline ~resolve code :> flow Html.elt list)
          | `N n -> to_html n
        in
        let doc =
          Utils.optional_elt
            Html.td ~a:(class_ ["doc"]) (block ~resolve doc)
        in
        let a, link = mk_anchor anchor in
        Html.tr ~a
          (Html.td ~a:(class_ attrs) (link @ content) :: doc)
      in
      Html.table (List.map one l)
      :: to_html rest
  in
  to_html t

(* This coercion is actually sound, but is not currently accepted by Tyxml.
   See https://github.com/ocsigen/tyxml/pull/265 for details
   Can be replaced by a simple type coercion once this is fixed
*)
let flow_to_item
  : flow Html.elt list -> item Html.elt list
  = fun x -> Html.totl @@ Html.toeltl x

let rec is_only_text l =
  let is_text : Item.t -> _ = function
    | Heading _ | Text _ -> true
    | Declaration _
      -> false
    | Nested { content = { items; _ }; _ }
      -> is_only_text items
  in
  List.for_all is_text l

let class_of_kind kind = match kind with
  | Some spec -> class_ ["spec"; spec]
  | None -> []

let should_coalesce = function
  | None -> false
  | Some s -> match s with
    | "exception" | "value" | "external"
    | "type" | "type-subst" | "extension"
    | "module-substitution"
    | "method" | "instance-variable" | "inherit"
      -> true
    | _ -> false

let items ~resolve l =
  let[@tailrec] rec walk_items
      ~only_text acc (t : Item.t list) : item Html.elt list =
    let continue_with rest elts =
      walk_items ~only_text (List.rev_append elts acc) rest
    in
    match t with
    | [] -> List.rev acc
    | Text _ :: _ as t ->
      let text, _, rest = Doctree.Take.until t ~classify:(function
        | Item.Text text -> Accum text
        | _ -> Stop_and_keep)
      in
      let content = flow_to_item @@ block ~resolve text in
      let elts = if only_text then
          content
        else
          [Html.aside (content :> any Html.elt list)]
      in
      elts
      |> continue_with rest
    | Heading h :: rest ->
      [heading ~resolve h]
      |> continue_with rest
    | Nested
        { kind; anchor; doc ; content = { summary; status; items = i } }
      :: rest ->
      let docs = (block ~resolve doc :> any Html.elt list) in
      let summary = inline ~resolve summary in
      let included_html =
        (items i :> any Html.elt list)
      in
      let content : any Html.elt list =
        let mk b =
          let a = if b then [Html.a_open ()] else [] in
          [Html.details ~a
              (Html.summary [Html.span ~a:[Html.a_class ["def"]] summary])
              included_html]
        in
        match status with
        | `Inline -> included_html
        | `Closed -> mk false
        | `Open -> mk true
        | `Default -> mk !Tree.open_details
      in
      let anchor_attrib, anchor_link = mk_anchor anchor in
      let a = class_of_kind kind @ anchor_attrib in
      (* TODO : Why double div ??? *)
      [Html.div [Html.div ~a
            (anchor_link @ [Html.div ~a:[Html.a_class ["doc"]]
                (docs @ content)])]]
      |> continue_with rest

    | Declaration { kind = kind0 ; _ } :: _ as t 
      when should_coalesce kind0 ->
      let l, doc, rest = Doctree.Take.until t ~classify:(function
        | Item.Declaration { doc = [] ; anchor ; content ; kind }
          when kind = kind0 ->
          Accum [(anchor, content, kind)]
        | Item.Declaration { kind; anchor; content; doc } when kind = kind0 ->
          Stop_and_accum ([(anchor, content, kind)], Some doc)
        | _ -> Stop_and_keep)
      in
      let content = List.map (fun (anchor, content, kind) ->
      let anchor_attrib, anchor_link = mk_anchor anchor in
        let a = class_of_kind kind @ anchor_attrib in
        let content = documentedSrc ~resolve content in
        Html.dt ~a (anchor_link @ content)
      ) l
      in
      let docs =
        match doc with
        | None | Some [] -> []
        | Some d -> [Html.dd (block ~resolve d)]
      in
      [Html.dl (content @ docs)]
      |> continue_with rest

    | Declaration {Item. kind; anchor ; content ; doc} :: rest ->
      let anchor_attrib, anchor_link = mk_anchor anchor in
      let a = class_of_kind kind @ anchor_attrib in
      let content = anchor_link @ documentedSrc ~resolve content in
      let elts = match doc with
        | [] ->
          [Html.div ~a content]
        | docs ->
          [Html.dl [
              Html.dt ~a content;
              Html.dd (block ~resolve docs);
            ]]
      in
      continue_with rest elts

  and items l = walk_items ~only_text:(is_only_text l) [] l in
  items l

module Toc = struct
  open Odoc_document.Doctree

  let render_toc (toc : Toc.t) =
    let rec section {Toc. anchor ; text ; children } =
      let text = inline_nolink text in
      let text =
        (text
         : non_link_phrasing Html.elt list
          :> (Html_types.flow5_without_interactive Html.elt) list)
      in
      let link =
        Html.a
          ~a:[Html.a_href ("#" ^ anchor)] text
      in
      match children with
      | [] -> [link]
      | _ -> [link; sections children]
    and sections the_sections =
      the_sections
      |> List.map (fun the_section -> Html.li (section the_section))
      |> Html.ul
    in
    match toc with
    | [] -> []
    | _ -> [Html.nav ~a:[Html.a_class ["toc"]] [sections toc]]

  let from_items i = render_toc @@ Toc.compute i
end

let rec subpage ?theme_uri
    ({Page. title; header; items = i ; subpages; url }) =
  let resolve = Link.Current url in
  let toc = Toc.from_items i in
  let header = items ~resolve header @ toc in
  let content = (items ~resolve i :> any Html.elt list) in
  let subpages = List.map (subpage ?theme_uri) subpages in
  let page =
    Tree.make ?theme_uri ~header ~url title content subpages
  in
  page

let render ?theme_uri page =
  subpage ?theme_uri page

let doc ~xref_base_uri b =
  let resolve = Link.Base xref_base_uri in
  block ~resolve b
