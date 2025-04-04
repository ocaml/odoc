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

[@@@warning "-26-27-32"]

open Odoc_utils

module HLink = Link
open Odoc_document.Types
module Doctree = Odoc_document.Doctree
module Url = Odoc_document.Url
module Link = HLink

module Md = struct
  include Cmarkit

  let meta = Cmarkit.Meta.none
end

let source fn (t : Source.t) =
  let rec token (x : Source.token) =
    match x with Elt i -> fn i | Tag (_, l) -> tokens l
  and tokens t = List.concat_map token t in
  tokens t

(* TODO: What's emph_level? *)
and styled style ~emph_level:_ content =
  match style with
  | `Bold ->
      let inlines_as_one_inline = Md.Inline.Inlines (content, Md.meta) in
      let emphasis = Md.Inline.Emphasis.make inlines_as_one_inline in
      [ Md.Inline.Strong_emphasis (emphasis, Md.meta) ]
  | `Italic | `Emphasis ->
      (* We treat emphasis as italic, since there's no difference in Markdown *)
      let inlines_as_one_inline = Md.Inline.Inlines (content, Md.meta) in
      let emphasis = Md.Inline.Emphasis.make inlines_as_one_inline in
      [ Md.Inline.Emphasis (emphasis, Md.meta) ]
  | `Superscript | `Subscript ->
      (* CommonMark doesn't have support for superscript/subscript, render the content as inline *)
      content

let entity = function "#45" -> "-" | "gt" -> ">" | e -> "&" ^ e ^ ";"

let rec inline_text_only inline =
  List.concat_map
    (fun (i : Inline.one) ->
      match i.desc with
      | Text "" -> []
      | Text s -> [ s ]
      | Entity s -> [ entity s ]
      | Linebreak -> []
      | Styled (_, content) -> inline_text_only content
      | Link { content; _ } -> inline_text_only content
      | Source s -> source inline_text_only s
      | _ -> [])
    inline

and block_text_only (blocks : Block.t) : string list =
  List.concat_map
    (fun (b : Block.one) ->
      match b.desc with
      | Paragraph inline | Inline inline -> inline_text_only inline
      | Source (_, s) -> source inline_text_only s
      | List (_, items) -> List.concat_map block_text_only items
      | Verbatim s -> [ s ]
      | _ -> [])
    blocks

and inline ~config ?(emph_level = 0) ~resolve (l : Inline.t) =
  let one (t : Inline.one) =
    match t.desc with
    | Text s -> [ Md.Inline.Text (s, Md.meta) ]
    | Entity s ->
        (* In CommonMark, HTML entities are supported directly, so we can just output them as text *)
        [ Md.Inline.Text (s, Md.meta) ]
    | Linebreak ->
        (* In CommonMark, a hard line break can be represented by a backslash followed by a newline or by two or more spaces at the end of a line. We use a hard break *)
        let break = Md.Inline.Break.make `Hard in
        [ Md.Inline.Break (break, Md.meta) ]
    | Styled (style, c) ->
        let inline_content = inline ~config ~emph_level ~resolve c in
        styled ~emph_level style inline_content
    | Link { target = External href; content; _ } ->
        let inline_content = inline ~config ~emph_level ~resolve content in
        let link_inline = Md.Inline.Inlines (inline_content, Md.meta) in
        let link_definition =
          Md.Link_definition.make ~dest:(href, Md.meta) ()
        in
        let link_reference = `Inline (link_definition, Md.meta) in
        let inline_link = Md.Inline.Link.make link_inline link_reference in
        [ Md.Inline.Link (inline_link, Md.meta) ]
    | Link { target = Internal internal; content; _ } ->
        let href =
          match internal with
          | Resolved uri ->
              (* TODO: Maybe internal links should be relative? *)
              let url = Link.href ~config ~resolve uri in
              (url, Md.meta)
          | Unresolved ->
              (* TODO: What's unresolved? A non-existing page/link? Do we want to raise or empty? *)
              ("", Md.meta)
        in
        let inline_content = inline ~config ~emph_level ~resolve content in
        let link_inline = Md.Inline.Inlines (inline_content, Md.meta) in
        let link_definition = Md.Link_definition.make ~dest:href () in
        let link_reference = `Inline (link_definition, Md.meta) in
        let inline_link = Md.Inline.Link.make link_inline link_reference in
        [ Md.Inline.Link (inline_link, Md.meta) ]
    | Source c ->
        (* CommonMark doesn't allow any complex node inside inline text, right now rendering inline nodes as text *)
        let content = String.concat ~sep:"" (source inline_text_only c) in
        [ Md.Inline.Code_span (Md.Inline.Code_span.of_string content, Md.meta) ]
    | Math s ->
        (* Since CommonMark doesn't support Math's, we just treat it as code. Maybe could use Ext_math_block or Ext_math_display *)
        [ Md.Inline.Code_span (Md.Inline.Code_span.of_string s, Md.meta) ]
    | Raw_markup (target, content) -> (
        match Astring.String.Ascii.lowercase target with
        | "html" ->
            let block_lines = Md.Block_line.tight_list_of_string content in
            [ Md.Inline.Raw_html (block_lines, Md.meta) ]
        | another_lang ->
            (* TODO: Is this correct? *)
            let msg =
              "Markdown only supports html blocks. There's a raw with "
              ^ another_lang
            in
            failwith msg)
  in
  List.concat_map one l

let heading ~config ~resolve (h : Heading.t) : Md.Block.t list =
  let inlines = inline ~config ~resolve h.title in
  let content = Md.Inline.Inlines (inlines, Md.meta) in
  let heading =
    Md.Block.Heading
      (Md.Block.Heading.make ~level:(h.level + 1) content, Md.meta)
  in
  [ heading ]

let rec block ~config ~resolve (l : Block.t) : Md.Block.t list =
  let one (t : Block.one) : Md.Block.t list =
    match t.desc with
    | Paragraph paragraph ->
        let inlines = inline ~config ~resolve paragraph in
        let inlines = Md.Inline.Inlines (inlines, Md.meta) in
        let paragraph_block =
          Md.Block.Paragraph (Md.Block.Paragraph.make inlines, Md.meta)
        in
        [ paragraph_block ]
    | List (typ, l) ->
        let list_type =
          match typ with
          | Ordered -> `Ordered (0, '.')
          | Unordered -> `Unordered '-'
        in
        let list_items =
          List.map
            (fun items ->
              let block = block ~config ~resolve items in
              let blocks = Md.Block.Blocks (block, Md.meta) in
              (Md.Block.List_item.make blocks, Md.meta))
            l
        in
        [
          (* TODO: Do we need to make it tight based on something? *)
          Md.Block.List
            (Md.Block.List'.make ~tight:true list_type list_items, Md.meta);
        ]
    | Inline i ->
        let inlines = Md.Inline.Inlines (inline ~config ~resolve i, Md.meta) in
        [ Md.Block.Paragraph (Md.Block.Paragraph.make inlines, Md.meta) ]
    | Table t ->
        let rows_data : (string * [ `Data | `Header ]) list list =
          match t.data with
          | [] -> []
          | rows ->
              List.map
                (fun (row : (Block.t * [ `Data | `Header ]) list) ->
                  List.map
                    (fun (content, cell_type) ->
                      let cell_text =
                        String.concat ~sep:" " (block_text_only content)
                      in
                      (cell_text, cell_type))
                    row)
                rows
        in

        (* If we have no data, return an empty paragraph *)
        if rows_data = [] then
          [
            Md.Block.Paragraph
              ( Md.Block.Paragraph.make (Md.Inline.Inlines ([], Md.meta)),
                Md.meta );
          ]
        else
          (* Find maximum number of columns across all rows *)
          let max_columns =
            List.fold_left
              (fun max_cols row ->
                let row_cols = List.length row in
                if row_cols > max_cols then row_cols else max_cols)
              0 rows_data
          in

          (* Find out if we have a header row *)
          let has_header_row =
            match rows_data with
            | first_row :: _ ->
                List.exists
                  (fun (_, cell_type) -> cell_type = `Header)
                  first_row
            | [] -> false
          in

          (* Helper to create a list with n elements *)
          let rec make_list n v =
            if n <= 0 then [] else v :: make_list (n - 1) v
          in

          (* Create table content with proper Markdown structure *)
          let header_cells, content_rows =
            match rows_data with
            | first_row :: rest when has_header_row ->
                (* Pad header cells to match max_columns *)
                let padded_header =
                  let cells = List.map fst first_row in
                  let missing = max_columns - List.length cells in
                  if missing > 0 then cells @ make_list missing "" else cells
                in
                (padded_header, rest)
            | _ ->
                (* No header - create an empty header matching the max columns *)
                (make_list max_columns "", rows_data)
          in

          let pad_row row =
            let cells = List.map fst row in
            let missing = max_columns - List.length cells in
            if missing > 0 then cells @ make_list missing "" else cells
          in

          (* Create the header row as inline text *)
          let header_inline =
            let header_text =
              "| " ^ String.concat ~sep:" | " header_cells ^ " |"
            in
            let header_md = Md.Inline.Text (header_text, Md.meta) in
            Md.Inline.Inlines ([ header_md ], Md.meta)
          in

          (* Create the separator row (based on column alignment) *)
          let separator_inline =
            (* Ensure alignment list is the right length *)
            let alignments =
              if List.length t.align >= max_columns then
                (* Take only the first max_columns elements *)
                let rec take n lst =
                  if n <= 0 then []
                  else match lst with [] -> [] | h :: t -> h :: take (n - 1) t
                in
                take max_columns t.align
              else
                (* Pad with defaults *)
                t.align
                @ make_list (max_columns - List.length t.align) Table.Default
            in

            let separator_cells =
              List.map
                (fun align ->
                  match align with
                  | Table.Left -> ":---"
                  | Table.Center -> ":---:"
                  | Table.Right -> "---:"
                  | Table.Default -> "---")
                alignments
            in
            let sep_text =
              "| " ^ String.concat ~sep:" | " separator_cells ^ " |"
            in
            let sep_md = Md.Inline.Text (sep_text, Md.meta) in
            Md.Inline.Inlines ([ sep_md ], Md.meta)
          in

          (* Create the content rows *)
          let content_inlines =
            List.map
              (fun row ->
                let cells = pad_row row in
                let row_text = "| " ^ String.concat ~sep:" | " cells ^ " |" in
                let row_md = Md.Inline.Text (row_text, Md.meta) in
                Md.Inline.Inlines ([ row_md ], Md.meta))
              content_rows
          in

          (* Build all rows in order: header, separator, content *)
          let table_inlines =
            [ header_inline; separator_inline ] @ content_inlines
          in

          (* Create paragraphs for each row *)
          List.map
            (fun inline ->
              Md.Block.Paragraph (Md.Block.Paragraph.make inline, Md.meta))
            table_inlines
    | Description l ->
        let item ({ key; definition; attr = _ } : Description.one) =
          let term = inline ~config ~resolve key in
          (* We extract definition as inline, since it came as "Block". There seems to be no way (in Cmarkit) to make it inline *)
          let definition_inline =
            Md.Inline.Text
              (String.concat ~sep:"" (block_text_only definition), Md.meta)
          in
          let space = Md.Inline.Text (" ", Md.meta) in
          let term_inline =
            Md.Inline.Inlines (term @ [ space; definition_inline ], Md.meta)
          in
          [ Md.Block.Paragraph (Md.Block.Paragraph.make term_inline, Md.meta) ]
        in
        List.concat_map item l
    | Verbatim s ->
        let code_snippet =
          Md.Block.Code_block
            (Md.Block.Code_block.make [ (s, Md.meta) ], Md.meta)
        in
        [ code_snippet ]
    | Source (lang_tag, s) ->
        let code_block =
          s |> source inline_text_only |> List.map (fun s -> (s, Md.meta))
        in
        let info_string = (lang_tag, Md.meta) in
        let code_snippet =
          Md.Block.Code_block
            (Md.Block.Code_block.make ~info_string code_block, Md.meta)
        in
        [ code_snippet ]
    | Math s ->
        (* Since CommonMark doesn't support Math's, we just treat it as code. Maybe could use Ext_math_block or Ext_math_display *)
        let block =
          Md.Block.Code_block
            (Md.Block.Code_block.make [ (s, Md.meta) ], Md.meta)
        in
        [ block ]
    | Raw_markup (target, content) -> (
        match Astring.String.Ascii.lowercase target with
        | "html" ->
            let block_lines = Md.Block_line.list_of_string content in
            [ Md.Block.Html_block (block_lines, Md.meta) ]
        | another_lang ->
            (* TODO: Is this correct? *)
            let msg =
              "Markdown only supports html blocks. There's a raw with "
              ^ another_lang
            in
            failwith msg)
    | Audio (_target, _alt) ->
        (* TODO: Raise a decent error here? Only saw assert false :( *)
        failwith "Audio isn't supported in markdown"
    | Video (_target, _alt) ->
        (* TODO: Raise a decent error here? Only saw assert false :( *)
        failwith "Video isn't supported in markdown"
    | Image (target, alt) ->
        let dest =
          match target with
          | Target.External url -> (url, Md.meta)
          | Target.Internal (Resolved uri) ->
              let url = Link.href ~config ~resolve uri in
              (url, Md.meta)
          | Target.Internal Unresolved ->
              (* TODO: What's unresolved? A non-existing page/link? *)
              ("", Md.meta)
        in
        let image =
          Md.Inline.Link.make
            (Md.Inline.Text (alt, Md.meta))
            (`Inline (Md.Link_definition.make ~dest (), Md.meta))
        in
        [
          Md.Block.Paragraph
            ( Md.Block.Paragraph.make
                (Md.Inline.Inlines
                   ([ Md.Inline.Image (image, Md.meta) ], Md.meta)),
              Md.meta );
        ]
  in
  List.concat_map one l

and items ~config ~resolve l : Md.Block.t list =
  let rec walk_items acc (t : Item.t list) =
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
        let content = block ~config ~resolve text in
        (continue_with [@tailcall]) rest content
    | Heading h :: rest ->
        (continue_with [@tailcall]) rest (heading ~config ~resolve h)
    | Include
        {
          attr = _attr;
          anchor = _anchor;
          source_anchor = _source_anchor;
          doc;
          content = { summary = _summary; status = _status; content = _content };
        }
      :: rest ->
        let content = block ~config ~resolve doc in
        (continue_with [@tailcall]) rest content
    | Declaration
        {
          Item.attr = _attr;
          anchor = _anchor;
          source_anchor = _source_anchor;
          content;
          doc;
        }
      :: rest ->
        let spec = documentedSrc ~config ~resolve content in
        let doc = block ~config ~resolve doc in
        let content = spec @ doc in
        (continue_with [@tailcall]) rest content
  and items l = walk_items [] l in
  items l

and documentedSrc ~config ~resolve (t : DocumentedSrc.t) =
  let open DocumentedSrc in
  let take_code l =
    Doctree.Take.until l ~classify:(fun x ->
        match (x : DocumentedSrc.one) with
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
  let rec to_markdown t : Md.Block.t list =
    match t with
    | [] -> []
    | (Code _ | Alternative _) :: _ ->
        let code, header, rest = take_code t in
        let info_string =
          match header with
          | Some header -> Some (header, Md.meta)
          | None -> None
        in
        let inline_source = source inline_text_only code in
        let code_block = [ (String.concat ~sep:"" inline_source, Md.meta) ] in
        let block =
          Md.Block.Code_block
            (Md.Block.Code_block.make ?info_string code_block, Md.meta)
        in
        [ block ] @ to_markdown rest
    | Subpage subp :: _ -> subpage ~config ~resolve subp
    | (Documented _ | Nested _) :: _ ->
        let l, _, rest = take_descr t in
        let one { DocumentedSrc.attrs = _; anchor = _; code; doc; markers = _ }
            =
          let content =
            match code with
            | `D code ->
                let inline_source = inline ~config ~resolve code in
                let inlines = Md.Inline.Inlines (inline_source, Md.meta) in
                let block =
                  Md.Block.Paragraph (Md.Block.Paragraph.make inlines, Md.meta)
                in
                [ block ]
            | `N n -> to_markdown n
          in

          let block_doc = block ~config ~resolve doc in
          List.append content block_doc
        in
        let all_blocks = List.concat_map one l in
        all_blocks @ to_markdown rest
  in
  to_markdown t

and subpage ~config ~resolve (subp : Subpage.t) =
  items ~config ~resolve subp.content.items

module Toc = struct
  open Odoc_document.Doctree
  open Types

  let on_sub : Subpage.status -> bool = function
    | `Closed | `Open | `Default -> false
    | `Inline -> true

  let gen_toc ~config ~resolve ~path i =
    let toc = Toc.compute path ~on_sub i in
    let rec section { Toc.url; text; children } =
      let _text = inline ~config ~resolve text in
      let title =
        (* (text) *)
        []
      in
      let title_str = "" in
      let href = Link.href ~config ~resolve url in
      { title; title_str; href; children = List.map section children }
    in
    List.map section toc
end

module Breadcrumbs = struct
  open Types

  let page_parent (page : Url.Path.t) =
    let page =
      match page with
      | { parent = Some parent; name = "index"; kind = `LeafPage } -> parent
      | _ -> page
    in
    match page with
    | { parent = None; name = "index"; kind = `LeafPage } -> None
    | { parent = Some parent; _ } -> Some parent
    | { parent = None; _ } ->
        Some { Url.Path.parent = None; name = "index"; kind = `LeafPage }

  let home_breadcrumb ~home_name:_ config ~current_path ~home_path =
    let href =
      Some
        (Link.href ~config ~resolve:(Current current_path)
           (Odoc_document.Url.from_path home_path))
    in
    { href; name = [ (* Html.txt home_name *) ]; kind = `LeafPage }

  let gen_breadcrumbs_no_sidebar ~config ~url =
    let url =
      match url with
      | { Url.Path.name = "index"; parent = Some parent; kind = `LeafPage } ->
          parent
      | _ -> url
    in
    match url with
    | { Url.Path.name = "index"; parent = None; kind = `LeafPage } ->
        let kind = `LeafPage in
        let current = { href = None; name = [ (* Html.txt "" *) ]; kind } in
        { parents = []; up_url = None; current }
    | url -> (
        (* This is the pre 3.0 way of computing the breadcrumbs *)
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
            Some
              (Link.href ~config ~resolve:(Current url)
                 (Odoc_document.Url.from_path path))
          in
          { href; name = [ (* Html.txt path.name *) ]; kind = path.kind }
        in
        let parent_paths =
          get_parent_paths (List.rev (Odoc_document.Url.Path.to_list url))
          |> List.rev
        in
        match List.rev parent_paths with
        | [] -> assert false
        | current :: parents ->
            let up_url =
              match page_parent current with
              | None -> None
              | Some up ->
                  Some
                    (Link.href ~config ~resolve:(Current url)
                       (Odoc_document.Url.from_path up))
            in
            let current = to_breadcrumb current in
            let parents = List.map to_breadcrumb parents |> List.rev in
            let home =
              home_breadcrumb ~home_name:"Index" config ~current_path:url
                ~home_path:
                  { Url.Path.name = "index"; parent = None; kind = `LeafPage }
            in
            { current; parents = home :: parents; up_url })

  let gen_breadcrumbs_with_sidebar ~config ~sidebar ~url:current_url =
    let find_parent =
      List.find_opt (function
        | ({ node = { url = { page; anchor = ""; _ }; _ }; _ } :
            Odoc_document.Sidebar.entry Tree.t)
          when Url.Path.is_prefix page current_url ->
            true
        | _ -> false)
    in
    let rec extract acc (tree : Odoc_document.Sidebar.t) =
      let parent =
        match find_parent tree with
        | Some { node = { url; valid_link; content = _; _ }; children } ->
            let href =
              if valid_link then
                Some (Link.href ~config ~resolve:(Current current_url) url)
              else None
            in
            (* let name = inline content in *)
            let name = [] in
            let breadcrumb = { href; name; kind = url.page.kind } in
            if url.page = current_url then Some (`Current breadcrumb)
            else Some (`Parent (breadcrumb, children))
        | _ -> None
      in
      match parent with
      | Some (`Parent (bc, children)) -> extract (bc :: acc) children
      | Some (`Current current) ->
          let up_url =
            List.find_map (fun (b : Types.breadcrumb) -> b.href) acc
          in
          { Types.current; parents = List.rev acc; up_url }
      | None ->
          let kind = current_url.kind and _name = current_url.name in
          let current = { href = None; name = [ (* Html.txt name *) ]; kind } in
          let up_url =
            List.find_map (fun (b : Types.breadcrumb) -> b.href) acc
          in
          let parents = List.rev acc in
          { Types.current; parents; up_url }
    in
    let escape = [] in
    extract escape sidebar

  let gen_breadcrumbs ~config ~sidebar ~url =
    match sidebar with
    | None -> gen_breadcrumbs_no_sidebar ~config ~url
    | Some sidebar -> gen_breadcrumbs_with_sidebar ~config ~sidebar ~url
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
    let { Page.preamble = _; items = i; url; source_anchor } =
      Doctree.Labels.disambiguate_page ~enter_subpages:false p
    in
    let subpages = subpages ~config ~sidebar @@ Doctree.Subpages.compute p in
    let resolve = Link.Current url in
    let breadcrumbs = Breadcrumbs.gen_breadcrumbs ~config ~sidebar ~url in
    let sidebar =
      (* match sidebar with
      | None -> None
      | Some sidebar ->
          let sidebar = Odoc_document.Sidebar.to_block sidebar url in
          (Some (block ~config ~resolve sidebar) :> any Html.elt list option) *)
      None
    in
    let i = Doctree.Shift.compute ~on_sub i in
    let uses_katex = Doctree.Math.has_math_elements p in
    let toc = Toc.gen_toc ~config ~resolve ~path:url i in
    let content = items ~config ~resolve i in
    let root_block = Md.Block.Blocks (content, Md.meta) in
    let doc = Cmarkit.Doc.make root_block in
    let header, preamble = Doctree.PageTitle.render_title ?source_anchor p in
    let header = items ~config ~resolve header in
    let preamble = items ~config ~resolve preamble in
    Markdown_page.make ~sidebar ~config ~header:(header @ preamble) ~toc
      ~breadcrumbs ~url ~uses_katex doc subpages

  and source_page ~config ~sidebar sp =
    (* TODO: I'm not enturely sure when this is called *)
    let { Source_page.url; contents = _ } = sp in
    let _resolve = Link.Current sp.url in
    let breadcrumbs = Breadcrumbs.gen_breadcrumbs ~config ~sidebar ~url in
    let sidebar = None in
    let title = url.Url.Path.name and doc = [ Md.Block.empty ] in
    let header = [] in
    Markdown_page.make_src ~breadcrumbs ~header ~config ~url ~sidebar title doc
end

let render ~(config : Config.t) ~sidebar = function
  (* .mld *)
  | Document.Page page -> [ Page.page ~config ~sidebar page ]
  (* .mli docs *)
  | Source_page src -> [ Page.source_page ~config ~sidebar src ]

let filepath ~config url = Link.Path.as_filename ~config url

let inline ~config ~xref_base_uri b =
  let resolve = Link.Base xref_base_uri in
  inline ~config ~resolve b
