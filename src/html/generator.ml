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
  [ Html.a ~a:[ Html.a_href ("#" ^ id); Html.a_class [ "anchor" ] ] [] ]

let mk_anchor anchor =
  match anchor with
  | None -> ([], [], [])
  | Some { Odoc_document.Url.Anchor.anchor; _ } ->
      let link = mk_anchor_link anchor in
      let extra_attr = [ Html.a_id anchor ] in
      let extra_class = [ "anchored" ] in
      (extra_attr, extra_class, link)

let class_ (l : Class.t) = if l = [] then [] else [ Html.a_class l ]

and raw_markup (t : Raw_markup.t) =
  let target, content = t in
  match Astring.String.Ascii.lowercase target with
  | "html" ->
      (* This is OK because we output *textual* HTML.
         In theory, we should try to parse the HTML with lambdasoup and rebuild
         the HTML tree from there.
      *)
      [ Html.Unsafe.data content ]
  | _ -> []

and source k ?a (t : Source.t) =
  let rec token (x : Source.token) =
    match x with
    | Elt i -> k i
    | Tag (None, l) ->
        let content = tokens l in
        if content = [] then [] else [ Html.span content ]
    | Tag (Some s, l) -> [ Html.span ~a:[ Html.a_class [ s ] ] (tokens l) ]
  and tokens t = Utils.list_concat_map t ~f:token in
  Utils.optional_elt Html.code ?a (tokens t)

and styled style ~emph_level =
  match style with
  | `Emphasis ->
      let a = if emph_level mod 2 = 0 then [] else [ Html.a_class [ "odd" ] ] in
      (emph_level + 1, Html.em ~a)
  | `Bold -> (emph_level, Html.b ~a:[])
  | `Italic -> (emph_level, Html.i ~a:[])
  | `Superscript -> (emph_level, Html.sup ~a:[])
  | `Subscript -> (emph_level, Html.sub ~a:[])

let rec internallink ~emph_level ~resolve ?(a = []) (t : InternalLink.t) =
  match t with
  | Resolved (uri, content) ->
      let href = Link.href ~resolve uri in
      let a = (a :> Html_types.a_attrib Html.attrib list) in
      let elt =
        Html.a ~a:(Html.a_href href :: a) (inline_nolink ~emph_level content)
      in
      let elt = (elt :> phrasing Html.elt) in
      [ elt ]
  | Unresolved content ->
      (* let title =
       *   Html.a_title (Printf.sprintf "unresolved reference to %S"
       *       (ref_to_string ref)
       * in *)
      let a = Html.a_class [ "xref-unresolved" ] :: a in
      let elt = Html.span ~a (inline ~emph_level ~resolve content) in
      let elt = (elt :> phrasing Html.elt) in
      [ elt ]

and internallink_nolink ~emph_level
    ~(a : Html_types.span_attrib Html.attrib list) (t : InternalLink.t) =
  match t with
  | Resolved (_, content) | Unresolved content ->
      [ Html.span ~a (inline_nolink ~emph_level content) ]

and inline ?(emph_level = 0) ~resolve (l : Inline.t) : phrasing Html.elt list =
  let one (t : Inline.one) =
    let a = class_ t.attr in
    match t.desc with
    | Text "" -> []
    | Text s ->
        if a = [] then [ Html.txt s ] else [ Html.span ~a [ Html.txt s ] ]
    | Entity s ->
        if a = [] then [ Html.entity s ] else [ Html.span ~a [ Html.entity s ] ]
    | Linebreak -> [ Html.br ~a () ]
    | Styled (style, c) ->
        let emph_level, app_style = styled style ~emph_level in
        [ app_style @@ inline ~emph_level ~resolve c ]
    | Link (href, c) ->
        let a = (a :> Html_types.a_attrib Html.attrib list) in
        let content = inline_nolink ~emph_level c in
        [ Html.a ~a:(Html.a_href href :: a) content ]
    | InternalLink c -> internallink ~emph_level ~resolve ~a c
    | Source c -> source (inline ~emph_level ~resolve) ~a c
    | Raw_markup r -> raw_markup r
  in
  Utils.list_concat_map ~f:one l

and inline_nolink ?(emph_level = 0) (l : Inline.t) :
    non_link_phrasing Html.elt list =
  let one (t : Inline.one) =
    let a = class_ t.attr in
    match t.desc with
    | Text "" -> []
    | Text s ->
        if a = [] then [ Html.txt s ] else [ Html.span ~a [ Html.txt s ] ]
    | Entity s ->
        if a = [] then [ Html.entity s ] else [ Html.span ~a [ Html.entity s ] ]
    | Linebreak -> [ Html.br ~a () ]
    | Styled (style, c) ->
        let emph_level, app_style = styled style ~emph_level in
        [ app_style @@ inline_nolink ~emph_level c ]
    | Link (_, c) -> inline_nolink ~emph_level c
    | InternalLink c -> internallink_nolink ~emph_level ~a c
    | Source c -> source (inline_nolink ~emph_level) ~a c
    | Raw_markup r -> raw_markup r
  in
  Utils.list_concat_map ~f:one l

let heading ~resolve (h : Heading.t) =
  let a, anchor =
    match h.label with
    | Some id -> ([ Html.a_id id ], mk_anchor_link id)
    | None -> ([], [])
  in
  let content = inline ~resolve h.title in
  let mk =
    match h.level with
    | 0 -> Html.h1
    | 1 -> Html.h2
    | 2 -> Html.h3
    | 3 -> Html.h4
    | 4 -> Html.h5
    | _ -> Html.h6
  in
  mk ~a (anchor @ content)

let rec block ~resolve (l : Block.t) : flow Html.elt list =
  let as_flow x = (x : phrasing Html.elt list :> flow Html.elt list) in
  let one (t : Block.one) =
    let mk_block ?(extra_class = []) mk content =
      let a = Some (class_ (extra_class @ t.attr)) in
      [ mk ?a content ]
    in
    match t.desc with
    | Inline i ->
        if t.attr = [] then as_flow @@ inline ~resolve i
        else mk_block Html.span (inline ~resolve i)
    | Paragraph i -> mk_block Html.p (inline ~resolve i)
    | List (typ, l) ->
        let mk = match typ with Ordered -> Html.ol | Unordered -> Html.ul in
        mk_block mk (List.map (fun x -> Html.li (block ~resolve x)) l)
    | Description l ->
        let item i =
          let a = class_ i.Description.attr in
          let term =
            (inline ~resolve i.Description.key
              : phrasing Html.elt list
              :> flow Html.elt list)
          in
          let def = block ~resolve i.Description.definition in
          Html.li ~a (term @ (Html.txt " " :: def))
        in
        mk_block Html.ul (List.map item l)
    | Raw_markup r -> raw_markup r
    | Verbatim s -> mk_block Html.pre [ Html.txt s ]
    | Source (lang_tag, c) ->
        let extra_class = [ "language-" ^ lang_tag ] in
        mk_block ~extra_class Html.pre (source (inline ~resolve) c)
  in
  Utils.list_concat_map l ~f:one

(* This coercion is actually sound, but is not currently accepted by Tyxml.
   See https://github.com/ocsigen/tyxml/pull/265 for details
   Can be replaced by a simple type coercion once this is fixed
*)
let flow_to_item : flow Html.elt list -> item Html.elt list =
 fun x -> Html.totl @@ Html.toeltl x

let div : ([< Html_types.div_attrib ], [< item ], [> Html_types.div ]) Html.star
    =
  Html.Unsafe.node "div"

let spec_class attr = class_ ("spec" :: attr)

let spec_doc_div ~resolve = function
  | [] -> []
  | docs ->
      let a = [ Html.a_class [ "spec-doc" ] ] in
      [ div ~a (flow_to_item @@ block ~resolve docs) ]

let rec documentedSrc ~resolve (t : DocumentedSrc.t) : item Html.elt list =
  let open DocumentedSrc in
  let take_code l =
    Doctree.Take.until l ~classify:(function
      | Code code -> Accum code
      | Alternative (Expansion { summary; _ }) -> Accum summary
      | _ -> Stop_and_keep)
  in
  let take_descr l =
    Doctree.Take.until l ~classify:(function
      | Documented { attrs; anchor; code; doc; markers } ->
          Accum
            [ { DocumentedSrc.attrs; anchor; code = `D code; doc; markers } ]
      | Nested { attrs; anchor; code; doc; markers } ->
          Accum
            [ { DocumentedSrc.attrs; anchor; code = `N code; doc; markers } ]
      | _ -> Stop_and_keep)
  in
  let rec to_html t : item Html.elt list =
    match t with
    | [] -> []
    | (Code _ | Alternative _) :: _ ->
        let code, _, rest = take_code t in
        source (inline ~resolve) code @ to_html rest
    | Subpage subp :: _ -> subpage ~resolve subp
    | (Documented _ | Nested _) :: _ ->
        let l, _, rest = take_descr t in
        let one { DocumentedSrc.attrs; anchor; code; doc; markers } =
          let content =
            match code with
            | `D code -> (inline ~resolve code :> item Html.elt list)
            | `N n -> to_html n
          in
          let doc =
            match doc with
            | [] -> []
            | doc ->
                let opening, closing = markers in
                let delim s =
                  [ Html.span ~a:(class_ [ "comment-delim" ]) [ Html.txt s ] ]
                in
                [
                  Html.div ~a:(class_ [ "def-doc" ])
                    (delim opening @ block ~resolve doc @ delim closing);
                ]
          in
          let extra_attr, extra_class, link = mk_anchor anchor in
          let content = (content :> any Html.elt list) in
          Html.li
            ~a:(extra_attr @ class_ (attrs @ extra_class))
            (link @ content @ doc)
        in
        Html.ol (List.map one l) :: to_html rest
  in
  to_html t

and subpage ~resolve (subp : Subpage.t) : item Html.elt list =
  items ~resolve subp.content.items

and items ~resolve l : item Html.elt list =
  let rec walk_items acc (t : Item.t list) : item Html.elt list =
    let continue_with rest elts =
      (walk_items [@tailcall]) (List.rev_append elts acc) rest
    in
    match t with
    | [] -> List.rev acc
    | Text _ :: _ as t ->
        let text, _, rest =
          Doctree.Take.until t ~classify:(function
            | Item.Text text -> Accum text
            | _ -> Stop_and_keep)
        in
        let content = flow_to_item @@ block ~resolve text in
        (continue_with [@tailcall]) rest content
    | Heading h :: rest ->
        (continue_with [@tailcall]) rest [ heading ~resolve h ]
    | Include { attr; anchor; doc; content = { summary; status; content } }
      :: rest ->
        let doc = spec_doc_div ~resolve doc in
        let included_html = (items content :> item Html.elt list) in
        let a_class =
          if List.length content = 0 then [ "odoc-include"; "shadowed-include" ]
          else [ "odoc-include" ]
        in
        let content : item Html.elt list =
          let details ~open' =
            let open' = if open' then [ Html.a_open () ] else [] in
            let summary =
              let extra_attr, extra_class, anchor_link = mk_anchor anchor in
              let a = spec_class (attr @ extra_class) @ extra_attr in
              Html.summary ~a @@ anchor_link @ source (inline ~resolve) summary
            in
            let inner =
              [
                Html.details ~a:open' summary
                  (included_html :> any Html.elt list);
              ]
            in
            [ Html.div ~a:[ Html.a_class a_class ] (doc @ inner) ]
          in
          match status with
          | `Inline -> doc @ included_html
          | `Closed -> details ~open':false
          | `Open -> details ~open':true
          | `Default -> details ~open':!Tree.open_details
        in
        (continue_with [@tailcall]) rest content
    | Declaration { Item.attr; anchor; content; doc } :: rest ->
        let extra_attr, extra_class, anchor_link = mk_anchor anchor in
        let a = spec_class (attr @ extra_class) @ extra_attr in
        let content = anchor_link @ documentedSrc ~resolve content in
        let spec =
          let doc = spec_doc_div ~resolve doc in
          [ div ~a:[ Html.a_class [ "odoc-spec" ] ] (div ~a content :: doc) ]
        in
        (continue_with [@tailcall]) rest spec
  and items l = walk_items [] l in
  items l

module Toc = struct
  open Odoc_document.Doctree

  let render_toc ~resolve (toc : Toc.t) =
    let rec section { Toc.url; text; children } =
      let text = inline_nolink text in
      let text =
        (text
          : non_link_phrasing Html.elt list
          :> Html_types.flow5_without_interactive Html.elt list)
      in
      let href = Link.href ~resolve url in
      let link = Html.a ~a:[ Html.a_href href ] text in
      match children with [] -> [ link ] | _ -> [ link; sections children ]
    and sections the_sections =
      the_sections
      |> List.map (fun the_section -> Html.li (section the_section))
      |> Html.ul
    in
    match toc with
    | [] -> []
    | _ -> [ Html.nav ~a:[ Html.a_class [ "odoc-toc" ] ] [ sections toc ] ]

  let on_sub : Subpage.status -> bool = function
    | `Closed | `Open | `Default -> false
    | `Inline -> true

  let from_items ~resolve ~path i =
    render_toc ~resolve @@ Toc.compute path ~on_sub i
end

module Page = struct
  let on_sub = function
    | `Page _ -> None
    | `Include x -> (
        match x.Include.status with
        | `Closed | `Open | `Default -> None
        | `Inline -> Some 0)

  let rec include_ ?theme_uri indent { Subpage.content; _ } =
    page ?theme_uri indent content

  and subpages ?theme_uri indent subpages =
    Utils.list_concat_map ~f:(include_ ?theme_uri indent) subpages

  and page ?theme_uri ?support_uri indent p : Odoc_document.Renderer.page list =
    let { Page.title; header; items = i; url } =
      Doctree.Labels.disambiguate_page p
    and subpages =
      (* Don't use the output of [disambiguate_page] to avoid unecessarily
         mangled labels. *)
      subpages ?theme_uri indent @@ Doctree.Subpages.compute p
    in
    let resolve = Link.Current url in
    let i = Doctree.Shift.compute ~on_sub i in
    let toc = Toc.from_items ~resolve ~path:url i in
    let header = items ~resolve header in
    let content = (items ~resolve i :> any Html.elt list) in
    Tree.make ?theme_uri ?support_uri ~indent ~header ~toc ~url title content
      subpages
end

let render ?theme_uri ?support_uri ~indent page =
  Page.page ?theme_uri ?support_uri indent page

let doc ~xref_base_uri b =
  let resolve = Link.Base xref_base_uri in
  block ~resolve b
