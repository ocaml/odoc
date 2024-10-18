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
module HLink = Link
open Odoc_document.Types
module Html = Tyxml.Html
module Doctree = Odoc_document.Doctree
module Url = Odoc_document.Url
module Link = HLink

type any = Html_types.flow5

type item = Html_types.flow5_without_header_footer

type flow = Html_types.flow5_without_sectioning_heading_header_footer

type phrasing = Html_types.phrasing

type non_link_phrasing = Html_types.phrasing_without_interactive

let mk_anchor_link id =
  [ Html.a ~a:[ Html.a_href ("#" ^ id); Html.a_class [ "anchor" ] ] [] ]

let mk_anchor config anchor =
  match anchor with
  | None -> ([], [], [])
  | _ when Config.search_result config ->
      (* When displaying for a search result, anchor are not added as it would
         make no sense to add them. *)
      ([], [], [])
  | Some { Url.Anchor.anchor; _ } ->
      let link = mk_anchor_link anchor in
      let extra_attr = [ Html.a_id anchor ] in
      let extra_class = [ "anchored" ] in
      (extra_attr, extra_class, link)

let mk_link_to_source ~config ~resolve anchor =
  match anchor with
  | None -> []
  | Some url ->
      let href = Link.href ~config ~resolve url in
      [
        Html.a
          ~a:[ Html.a_href href; Html.a_class [ "source_link" ] ]
          [ Html.txt "Source" ];
      ]

let class_ (l : Class.t) = if l = [] then [] else [ Html.a_class l ]

let inline_math (s : Math.t) =
  Html.code ~a:[ Html.a_class [ "odoc-katex-math" ] ] [ Html.txt s ]

let block_math (s : Math.t) =
  Html.pre ~a:[ Html.a_class [ "odoc-katex-math"; "display" ] ] [ Html.txt s ]

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
  and tokens t = Odoc_utils.List.concat_map t ~f:token in
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

let rec internallink ~config ~emph_level ~resolve ?(a = []) target content
    tooltip =
  let a = match tooltip with Some s -> Html.a_title s :: a | None -> a in
  let elt =
    match target with
    | Target.Resolved uri ->
        let href = Link.href ~config ~resolve uri in
        let content = inline_nolink ~emph_level content in
        if Config.search_result config then
          (* When displaying for a search result, links are displayed as regular
             text. *)
          Html.span ~a content
        else
          let a =
            Html.a_href href :: (a :> Html_types.a_attrib Html.attrib list)
          in
          Html.a ~a content
    | Unresolved ->
        (* let title =
         *   Html.a_title (Printf.sprintf "unresolved reference to %S"
         *       (ref_to_string ref)
         * in *)
        let a = Html.a_class [ "xref-unresolved" ] :: a in
        Html.span ~a (inline ~config ~emph_level ~resolve content)
  in
  [ (elt :> phrasing Html.elt) ]

and inline ~config ?(emph_level = 0) ~resolve (l : Inline.t) :
    phrasing Html.elt list =
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
        [ app_style @@ inline ~config ~emph_level ~resolve c ]
    | Link { content = c; _ } when Config.search_result config ->
        (* When displaying for a search result, links are displayed as regular
           text. *)
        let content = inline_nolink ~emph_level c in
        [ Html.span ~a content ]
    | Link { target = External href; content = c; _ } ->
        let a = (a :> Html_types.a_attrib Html.attrib list) in
        let content = inline_nolink ~emph_level c in
        [ Html.a ~a:(Html.a_href href :: a) content ]
    | Link { target = Internal t; content; tooltip } ->
        internallink ~config ~emph_level ~resolve ~a t content tooltip
    | Source c -> source (inline ~config ~emph_level ~resolve) ~a c
    | Math s -> [ inline_math s ]
    | Raw_markup r -> raw_markup r
  in
  Odoc_utils.List.concat_map ~f:one l

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
    | Link _ -> assert false
    | Source c -> source (inline_nolink ~emph_level) ~a c
    | Math s -> [ inline_math s ]
    | Raw_markup r -> raw_markup r
  in
  Odoc_utils.List.concat_map ~f:one l

let heading ~config ~resolve (h : Heading.t) =
  let a, anchor =
    match h.label with
    | Some _ when Config.search_result config ->
        (* When displaying for a search result, anchor are not added as it would
           make no sense to add them. *)
        ([], [])
    | Some id -> ([ Html.a_id id ], mk_anchor_link id)
    | None -> ([], [])
  in
  let content = inline ~config ~resolve h.title in
  let source_link = mk_link_to_source ~config ~resolve h.source_anchor in
  let mk =
    match h.level with
    | 0 -> Html.h1
    | 1 -> Html.h2
    | 2 -> Html.h3
    | 3 -> Html.h4
    | 4 -> Html.h5
    | _ -> Html.h6
  in
  mk ~a (anchor @ content @ source_link)

let text_align = function
  | Table.Left -> [ Html.a_style "text-align:left" ]
  | Center -> [ Html.a_style "text-align:center" ]
  | Right -> [ Html.a_style "text-align:right" ]
  | Default -> []

let cell_kind = function `Header -> Html.th | `Data -> Html.td

let rec block ~config ~resolve (l : Block.t) : flow Html.elt list =
  let as_flow x = (x : phrasing Html.elt list :> flow Html.elt list) in
  let one (t : Block.one) =
    let mk_block ?(extra_class = []) mk content =
      let a = Some (class_ (extra_class @ t.attr)) in
      [ mk ?a content ]
    in
    let mk_media_block media_block target alt =
      let block =
        match target with
        | Target.External url -> media_block url alt
        | Internal (Resolved uri) ->
            let url = Link.href ~config ~resolve uri in
            media_block url alt
        | Internal Unresolved ->
            let content = [ Html.txt alt ] in
            let a = Html.a_class [ "xref-unresolved" ] :: [] in
            [ Html.span ~a content ]
      in
      mk_block Html.div block
    in
    match t.desc with
    | Inline i ->
        if t.attr = [] then as_flow @@ inline ~config ~resolve i
        else mk_block Html.span (inline ~config ~resolve i)
    | Paragraph i -> mk_block Html.p (inline ~config ~resolve i)
    | List (typ, l) ->
        let mk = match typ with Ordered -> Html.ol | Unordered -> Html.ul in
        mk_block mk (List.map (fun x -> Html.li (block ~config ~resolve x)) l)
    | Table t ->
        mk_block ~extra_class:[ "odoc-table" ]
          (fun ?a x -> Html.table ?a x)
          (mk_rows ~config ~resolve t)
    | Description l ->
        let item i =
          let a = class_ i.Description.attr in
          let term =
            (inline ~config ~resolve i.Description.key
              : phrasing Html.elt list
              :> flow Html.elt list)
          in
          let def = block ~config ~resolve i.Description.definition in
          Html.li ~a (term @ (Html.txt " " :: def))
        in
        mk_block Html.ul (List.map item l)
    | Raw_markup r -> raw_markup r
    | Verbatim s -> mk_block Html.pre [ Html.txt s ]
    | Source (lang_tag, c) ->
        let extra_class = [ "language-" ^ lang_tag ] in
        mk_block ~extra_class Html.pre (source (inline ~config ~resolve) c)
    | Math s -> mk_block Html.div [ block_math s ]
    | Audio (target, alt) ->
        let audio src alt =
          [
            Html.audio ~src
              ~a:[ Html.a_controls (); Html.a_aria "label" [ alt ] ]
              [];
          ]
        in
        mk_media_block audio target alt
    | Video (target, alt) ->
        let video src alt =
          [
            Html.video ~src
              ~a:[ Html.a_controls (); Html.a_aria "label" [ alt ] ]
              [];
          ]
        in
        mk_media_block video target alt
    | Image (target, alt) ->
        let image src alt =
          let img =
            Html.a
              ~a:[ Html.a_href src; Html.a_class [ "img-link" ] ]
              [ Html.img ~src ~alt () ]
          in
          [ img ]
        in
        mk_media_block image target alt
  in

  Odoc_utils.List.concat_map l ~f:one

and mk_rows ~config ~resolve { align; data } =
  let mk_row row =
    let mk_cell ~align (x, h) =
      let a = text_align align in
      cell_kind ~a h (block ~config ~resolve x)
    in
    let alignment align =
      match align with align :: q -> (align, q) | [] -> (Table.Default, [])
      (* Second case is for recovering from a too short alignment list. A
         warning should have been raised when loading the doc-comment. *)
    in
    let acc, _align =
      List.fold_left
        (fun (acc, aligns) (x, h) ->
          let align, aligns = alignment aligns in
          let cell = mk_cell ~align (x, h) in
          (cell :: acc, aligns))
        ([], align) row
    in
    Html.tr (List.rev acc)
  in
  List.map mk_row data

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

let spec_doc_div ~config ~resolve = function
  | [] -> []
  | docs ->
      let a = [ Html.a_class [ "spec-doc" ] ] in
      [ div ~a (flow_to_item @@ block ~config ~resolve docs) ]

let rec documentedSrc ~config ~resolve (t : DocumentedSrc.t) :
    item Html.elt list =
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
        source (inline ~config ~resolve) code @ to_html rest
    | Subpage subp :: _ -> subpage ~config ~resolve subp
    | (Documented _ | Nested _) :: _ ->
        let l, _, rest = take_descr t in
        let one { DocumentedSrc.attrs; anchor; code; doc; markers } =
          let content =
            match code with
            | `D code -> (inline ~config ~resolve code :> item Html.elt list)
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
                    (delim opening @ block ~config ~resolve doc @ delim closing);
                ]
          in
          let extra_attr, extra_class, link = mk_anchor config anchor in
          let content = (content :> any Html.elt list) in
          Html.li
            ~a:(extra_attr @ class_ (attrs @ extra_class))
            (link @ content @ doc)
        in
        Html.ol (List.map one l) :: to_html rest
  in
  to_html t

and subpage ~config ~resolve (subp : Subpage.t) : item Html.elt list =
  items ~config ~resolve subp.content.items

and items ~config ~resolve l : item Html.elt list =
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
        let content = flow_to_item @@ block ~config ~resolve text in
        (continue_with [@tailcall]) rest content
    | Heading h :: rest ->
        (continue_with [@tailcall]) rest [ heading ~config ~resolve h ]
    | Include
        {
          attr;
          anchor;
          source_anchor;
          doc;
          content = { summary; status; content };
        }
      :: rest ->
        let doc = spec_doc_div ~config ~resolve doc in
        let included_html = (items content :> item Html.elt list) in
        let a_class =
          if List.length content = 0 then [ "odoc-include"; "shadowed-include" ]
          else [ "odoc-include" ]
        in
        let content : item Html.elt list =
          let details ~open' =
            let open' = if open' then [ Html.a_open () ] else [] in
            let summary =
              let extra_attr, extra_class, anchor_link =
                mk_anchor config anchor
              in
              let link_to_source =
                mk_link_to_source ~config ~resolve source_anchor
              in
              let a = spec_class (attr @ extra_class) @ extra_attr in
              Html.summary ~a @@ anchor_link @ link_to_source
              @ source (inline ~config ~resolve) summary
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
          | `Default -> details ~open':(Config.open_details config)
        in
        (continue_with [@tailcall]) rest content
    | Declaration { Item.attr; anchor; source_anchor; content; doc } :: rest ->
        let extra_attr, extra_class, anchor_link = mk_anchor config anchor in
        let link_to_source = mk_link_to_source ~config ~resolve source_anchor in
        let a = spec_class (attr @ extra_class) @ extra_attr in
        let content =
          anchor_link @ link_to_source @ documentedSrc ~config ~resolve content
        in
        let spec =
          let doc = spec_doc_div ~config ~resolve doc in
          [ div ~a:[ Html.a_class [ "odoc-spec" ] ] (div ~a content :: doc) ]
        in
        (continue_with [@tailcall]) rest spec
  and items l = walk_items [] l in
  items l

module Toc = struct
  open Odoc_document.Doctree
  open Types

  let on_sub : Subpage.status -> bool = function
    | `Closed | `Open | `Default -> false
    | `Inline -> true

  let gen_toc ~config ~resolve ~path i =
    let toc = Toc.compute path ~on_sub i in
    let rec section { Toc.url; text; children } =
      let text = inline_nolink text in
      let title =
        (text
          : non_link_phrasing Html.elt list
          :> Html_types.flow5_without_interactive Html.elt list)
      in
      let title_str =
        List.map (Format.asprintf "%a" (Tyxml.Html.pp_elt ())) text
        |> String.concat ""
      in
      let href = Link.href ~config ~resolve url in
      { title; title_str; href; children = List.map section children }
    in
    List.map section toc
end

module Breadcrumbs = struct
  open Types

  let gen_breadcrumbs ~config ~url =
    let rec get_parent_paths x =
      match x with
      | [] -> []
      | x :: xs -> (
          match Odoc_document.Url.Path.of_list (List.rev (x :: xs)) with
          | Some x -> x :: get_parent_paths xs
          | None -> get_parent_paths xs)
    in
    let to_breadcrumb path =
      let href =
        Link.href ~config ~resolve:(Current url)
          (Odoc_document.Url.from_path path)
      in
      { href; name = path.name; kind = path.kind }
    in
    get_parent_paths (List.rev (Odoc_document.Url.Path.to_list url))
    |> List.rev |> List.map to_breadcrumb
end

module Page = struct
  let on_sub = function
    | `Page _ -> None
    | `Include x -> (
        match x.Include.status with
        | `Closed | `Open | `Default -> None
        | `Inline -> Some 0)

  let rec include_ ~config ~sidebar { Subpage.content; _ } =
    page ~config ~sidebar content

  and subpages ~config ~sidebar subpages =
    List.map (include_ ~config ~sidebar) subpages

  and page ~config ~sidebar p : Odoc_document.Renderer.page =
    let { Page.preamble; items = i; url; source_anchor } =
      Doctree.Labels.disambiguate_page ~enter_subpages:false p
    in
    let subpages = subpages ~config ~sidebar @@ Doctree.Subpages.compute p in
    let resolve = Link.Current url in
    let sidebar =
      match sidebar with
      | None -> None
      | Some sidebar ->
          let sidebar = Odoc_document.Sidebar.to_block sidebar url in
          (* let sidebar = Odoc_document.Sidebar.to_block sidebar p in *)
          (Some (block ~config ~resolve sidebar) :> any Html.elt list option)
    in
    let i = Doctree.Shift.compute ~on_sub i in
    let uses_katex = Doctree.Math.has_math_elements p in
    let toc = Toc.gen_toc ~config ~resolve ~path:url i in
    let breadcrumbs = Breadcrumbs.gen_breadcrumbs ~config ~url in
    let content = (items ~config ~resolve i :> any Html.elt list) in
    if Config.as_json config then
      let source_anchor =
        match source_anchor with
        | Some url -> Some (Link.href ~config ~resolve url)
        | None -> None
      in
      Html_fragment_json.make ~sidebar ~config
        ~preamble:(items ~config ~resolve preamble :> any Html.elt list)
        ~breadcrumbs ~toc ~url ~uses_katex ~source_anchor content subpages
    else
      let header =
        items ~config ~resolve
          (Doctree.PageTitle.render_title ?source_anchor p @ preamble)
      in
      Html_page.make ~sidebar ~config ~header ~toc ~breadcrumbs ~url ~uses_katex
        content subpages

  and source_page ~config sp =
    let { Source_page.url; contents } = sp in
    let resolve = Link.Current sp.url in
    let title = url.Url.Path.name
    and doc = Html_source.html_of_doc ~config ~resolve contents in
    let breadcrumbs = Breadcrumbs.gen_breadcrumbs ~config ~url in
    let header =
      items ~config ~resolve (Doctree.PageTitle.render_src_title sp)
    in
    if Config.as_json config then
      Html_fragment_json.make_src ~config ~url ~breadcrumbs [ doc ]
    else Html_page.make_src ~breadcrumbs ~header ~config ~url title [ doc ]
end

let render ~config ~sidebar = function
  | Document.Page page -> [ Page.page ~config ~sidebar page ]
  | Source_page src -> [ Page.source_page ~config src ]

let filepath ~config url = Link.Path.as_filename ~config url

let doc ~config ~xref_base_uri b =
  let resolve = Link.Base xref_base_uri in
  block ~config ~resolve b
