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

module Location = Odoc_model.Location_
module Paths = Odoc_model.Paths
module Html = Tyxml.Html

type item = Html_types.flow5
type flow = Html_types.flow5_without_header_footer
type flow_no_heading = Html_types.flow5_without_sectioning_heading_header_footer
type phrasing = Html_types.phrasing
type non_link_phrasing = Html_types.phrasing_without_interactive

let anchor_link anchor =
  [ Html.a ~a:[Html.a_href ("#" ^ anchor); Html.a_class ["anchor"]] []]

let anchor_attrib anchor =
  [ Html.a_id anchor; Html.a_class ["anchored"] ]

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
    let href = Tree.Link.href ~resolve uri in
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

let heading ~resolve ~a (h : Heading.t) =
  let a, anchor = match h.label with
    | Some id -> Html.a_id id :: a, anchor_link id
    | None -> a, []
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


let rec block_no_heading
    ~resolve (l: Block.t) : flow_no_heading Html.elt list =
  let as_flow x =
    (x : phrasing Html.elt list :> [> flow_no_heading] Html.elt list)
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
      [mk ~a (List.map
            (fun x -> Html.li (block_no_heading ~resolve x)) l)]
    | Description l ->
      [Html.dl ~a (Utils.list_concat_map l ~f:(fun (i,b) ->
          let i = as_flow @@ inline ~resolve i in
          [Html.dt i ; Html.dd (block_no_heading ~resolve b) ]
        ))]
    | Heading _h -> [] (* What should we do here ? *)
    | Raw_markup r ->
      raw_markup r
    | Verbatim s ->
      [Html.pre ~a [Html.txt s]]
    | Source c ->
      as_flow @@ source (inline ~resolve) ~a c
  in 
  Utils.list_concat_map l ~f:one

let block ~resolve (l: Block.t) : [> flow] Html.elt list =
  let as_flow x =
    (x : phrasing Html.elt list :> [> flow] Html.elt list)
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
      [mk ~a (List.map (fun x -> Html.li (block_no_heading ~resolve x)) l)]
    | Description l ->
      [Html.dl ~a (Utils.list_concat_map l ~f:(fun (i,b) ->
          let i =
            (inline ~resolve i
             : phrasing Html.elt list
              :> flow_no_heading Html.elt list)
          in
          [Html.dt i ; Html.dd (block_no_heading ~resolve b) ]
        ))]
    | Heading h ->
      [heading ~resolve ~a h]
    | Raw_markup r ->
      raw_markup r
    | Verbatim s ->
      [Html.pre ~a [Html.txt s]]
    | Source c ->
      [Html.pre ~a (source (inline ~resolve) c)]
  in 
  Utils.list_concat_map l ~f:one


let documentedSrc ~resolve (t : DocumentedSrc.t) =
  let rec coalece acc ?current (content : DocumentedSrc.t) =
    let (+:?) x l = Utils.fold_option ~none:l ~some:(fun x -> x :: l) x in
    match current, content with
    | current, [] ->
      List.rev (current +:? acc)
    | Some `O (attr', code'), Code { attr ; code } :: content
      when attr = attr' ->
      coalece acc ~current:(`O (attr, code' @ code)) content
    | Some `T l, Documented { attrs; anchor; code; doc } :: content ->
      let code = `D code in
      let x = {DocumentedSrc. attrs ; anchor ; code ; doc } in
      coalece acc ~current:(`T (l @ [x])) content
    | Some `T l, Nested { attrs; anchor; code; doc } :: content ->
      let code = `N (coalece [] code) in
      let x = {DocumentedSrc. attrs ; anchor ; code ; doc } in
      coalece acc ~current:(`T (l @ [x])) content
    | current , Code { attr ; code } :: content ->
      coalece (current +:? acc) ~current:(`O (attr, code)) content
    | current , Documented { attrs; anchor; code; doc } :: content ->
      let code = `D code in
      let x = {DocumentedSrc. attrs ; anchor ; code ; doc } in
      coalece (current +:? acc) ~current:(`T [x]) content
    | current , Nested { attrs; anchor; code; doc } :: content ->
      let code = `N (coalece [] code) in
      let x = {DocumentedSrc. attrs ; anchor ; code ; doc } in
      coalece (current +:? acc) ~current:(`T [x]) content
  in
  let rec to_html t : flow_no_heading Html.elt list =
    Utils.list_concat_map t ~f:(function 
      | `O (attr, code) ->
        let a = class_ attr in
        source (inline ~resolve) ~a code
      | `T l ->
        let one {DocumentedSrc. attrs ; anchor ; code ; doc } =
          let content = match code with
            | `D code ->
              (inline ~resolve code :> flow_no_heading Html.elt list)
            | `N n -> to_html n
          in
          let doc = match doc with
            | [] -> []
            | doc ->
              [Html.td ~a:(class_ ["doc"])
                  (block_no_heading ~resolve doc)]
          in
          Html.tr ~a:(anchor_attrib anchor)
            (Html.td ~a:(class_ attrs)
                (anchor_link anchor @ content) :: doc)
        in
        [Html.table (List.map one l)]
    )
  in
  to_html @@ coalece [] t

let rec item ~resolve (t : Item.t) =
  let as_item x = (x : flow Html.elt list :> item Html.elt list) in
  match t with
  | Text content ->
    [Html.aside (as_item @@ block ~resolve content)]
  | Section (header, content) ->
    let h = block ~resolve header in
    [Html.section (Html.header h :: items ~resolve content)]
  | Nested
      ({ attr; anchor; content = { summary; status; items = i } }, docs)
    ->
    let docs = as_item @@ block ~resolve docs in
    let summary = inline ~resolve summary in
    let included_html = items ~resolve i in
    let content =
      let mk b =
        let a = if b then [Html.a_open ()] else [] in
        [Html.details ~a
            (Html.summary [Html.span ~a:[Html.a_class ["def"]] summary])
            included_html]
      in
      match status with
      | `Inline ->
        included_html
      | `Closed -> mk false        
      | `Open -> mk true
      | `Default -> mk !Tree.open_details
    in
    let anchor_attrib, anchor_link = match anchor with
      | Some a -> anchor_attrib a, anchor_link a
      | None -> [], []
    in
    let a = class_ (["spec"; "include"] @ attr) @ anchor_attrib in
    (* TODO : Why double div ??? *)
    [Html.div [Html.div ~a
          (anchor_link @ [Html.div ~a:[Html.a_class ["doc"]] (docs @ content)])
    ]]
  | Declaration ({Item. attr; anchor ; content}, []) ->
    let anchor_attrib, anchor_link = match anchor with
      | Some a -> anchor_attrib a, anchor_link a
      | None -> [], []
    in
    let a = class_ attr @ anchor_attrib in
    let content = documentedSrc ~resolve content in
    [Html.div ~a (anchor_link @ content)]
  | Declaration ({Item. attr; anchor ; content}, docs) ->
    let anchor_attrib, anchor_link = match anchor with
      | Some a -> anchor_attrib a, anchor_link a
      | None -> [], []
    in
    let a = class_ attr @ anchor_attrib in
    let content = documentedSrc ~resolve content in
    let docs =
      Utils.optional_elt Html.dd (block_no_heading ~resolve docs)
    in
    [Html.dl (Html.dt ~a (anchor_link @ content) :: docs)]
  | Declarations (l, docs) -> 
    let content = List.map (fun {Item. attr; anchor ; content} ->
      let anchor_attrib, anchor_link = match anchor with
        | Some a -> anchor_attrib a, anchor_link a
        | None -> [], []
      in
      let a = class_ attr @ anchor_attrib in
      let content = documentedSrc ~resolve content in
      Html.dt ~a (anchor_link @ content)
    ) l
    in 
    let docs =
      Utils.optional_elt Html.dd (block_no_heading ~resolve docs)
    in
    [Html.dl (content @ docs)]

and items ~resolve l = Utils.list_concat_map ~f:(item ~resolve) l


let rec coalece_items acc ?current (item : Item.t list) =
  let (+:?) x l = Utils.fold_option ~none:l ~some:(fun x -> x :: l) x in
  match current, item with
  | current, [] ->
    List.rev (current +:? acc)
  | Some Item.Text text0, Text text :: content ->
    coalece_items acc ~current:(Text (text0 @ text)) content
  | current , Text text :: content ->
    coalece_items (current +:? acc) ~current:(Item.Text text) content
  | current , i :: content ->
    coalece_items (current +:? acc) ~current:i content

let page_content ~resolve l = 
  match coalece_items [] l with
  | [Item.Text t] -> (block ~resolve t :> item Html.elt list)
  | l -> items ~resolve l

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

let rec subpage ?theme_uri
    ({Page. title; header; items = i ; toc; subpages; url }) =
  let resolve = Tree.Link.Current url in
  let header_docs =
    block ~resolve header @ render_toc toc
  in
  let content = page_content ~resolve i in
  let subpages = List.map (subpage ?theme_uri) subpages in
  let page =
    Tree.make ?theme_uri ~header_docs ~url title content subpages
  in
  page

let render ?theme_uri page =
  subpage ?theme_uri page

let doc ~xref_base_uri b =
  let resolve = Tree.Link.Base xref_base_uri in
  (block ~resolve b :> item Html.elt list)
