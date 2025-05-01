let strf = Printf.sprintf

(* ocamlmark parsing *)

open Odoc_parser
open Cmarkit

(* Text location and comment massaging.

   One slight annoyance is that CommonMark is sensitive to leading
   blanks on lines and ocamldoc comments are usually indented by [n]
   spaces up the … of (** … *). So we can't just feed it the comment
   text: we would mostly get CommonMark indented code blocks.

   So we massage the comment to trim up to [n] initial spaces after
   newlines. [n] being the number of columns until … in (** … *). We
   need to remember how much we trimmed on each line in order to patch
   the locations reported by cmarkit. Below we keep pass that info
   around using the [~locator] argument.

   This is not needed in [md] files, but the code is kept in case we
   add support for markdown in docstrings. *)

let comment_col ~location = location.Lexing.pos_cnum - location.Lexing.pos_bol

let massage_comment ~location b s =
  let rec next_non_space s ~max i =
    if i > max || not (s.[i] = ' ') then i else next_non_space s ~max (i + 1)
  in
  let rec find_after_trim ~max_trim s max ~start i =
    if i - start + 1 > max_trim || i > max || s.[i] <> ' ' then i
    else find_after_trim ~max_trim s max ~start (i + 1)
  in
  let flush b s start last =
    Buffer.add_substring b s start (last - start + 1)
  in
  let rec loop b s acc ~max_trim max start k =
    if k > max then (
      flush b s start max;
      ((location, Array.of_list (List.rev acc)), Buffer.contents b))
    else if not (s.[k] = '\n' || s.[k] = '\r') then
      loop b s acc ~max_trim max start (k + 1)
    else
      let next = k + 1 in
      let next =
        if s.[k] = '\r' && next <= max && s.[next] = '\n' then next + 1
        else next
      in
      let after_trim = find_after_trim ~max_trim s max ~start:next next in
      let trim = after_trim - next in
      flush b s start (next - 1);
      loop b s (trim :: acc) ~max_trim max after_trim after_trim
  in
  if s = "" then ((location, [| 0 |]), s)
  else
    let max = String.length s - 1 in
    let nsp = next_non_space s ~max 0 in
    let max_trim = comment_col ~location + nsp in
    loop b s [ nsp (* trim *) ] ~max_trim max nsp nsp

let textloc_to_loc ~locator textloc =
  (* Note: if you get an [Invalid_argument] from this function suspect a bug
     in cmarkit's location computation. *)
  let point_of_line_and_byte_pos ~locator:(location, line_trim_counts) l pos =
    let line_num, line_pos = l in
    let line = location.Lexing.pos_lnum + line_num - 1 in
    try
      let column = line_trim_counts.(line_num - 1) + (pos - line_pos) in
      let column =
        match line_num with 1 -> comment_col ~location + column | _ -> column
      in
      { Loc.line; column }
    with _ ->
      (* Presumably this is the above-mentioned bug that's being hit. *)
      { Loc.line = -1; column = -1 }
  in
  let file = Textloc.file textloc in
  let first_line = Textloc.first_line textloc in
  let first_byte = Textloc.first_byte textloc in
  let last_line = Textloc.last_line textloc in
  let last_byte = Textloc.last_byte textloc + 1 in
  let start = point_of_line_and_byte_pos ~locator first_line first_byte in
  let end_ = point_of_line_and_byte_pos ~locator last_line last_byte in
  { Loc.file; start; end_ }

let meta_to_loc ~locator meta = textloc_to_loc ~locator (Meta.textloc meta)

(* Sometimes we need to munge a bit the cmarkit metas and textlocs.
   These function do that. They are not general and make assumptions
   about the nature of data they apply to. E.g. most assume the
   textloc is on the same line. *)

let chop_end_of_meta_textloc ~count meta =
  let textloc = Meta.textloc meta in
  let last_line = Textloc.last_line textloc in
  let last_byte = Textloc.last_byte textloc - count in
  let textloc = Textloc.set_last textloc ~last_byte ~last_line in
  Meta.with_textloc ~keep_id:true meta textloc

let split_info_string_locs ~left_count ~right_count m =
  if right_count = 0 then (Meta.textloc m, Textloc.none)
  else
    let textloc = Meta.textloc m in
    let line = Textloc.first_line textloc in
    let last_byte = Textloc.first_byte textloc + left_count - 1 in
    let first_byte = Textloc.last_byte textloc - right_count + 1 in
    ( Textloc.set_last textloc ~last_byte ~last_line:line,
      Textloc.set_first textloc ~first_byte ~first_line:line )

let textloc_of_sub textloc ~first ~last (* in textloc relative space *) =
  let file = Textloc.file textloc in
  let line = Textloc.first_line textloc in
  let first_byte = Textloc.first_byte textloc + first in
  let last_byte = Textloc.first_byte textloc + last in
  Textloc.v ~file ~first_byte ~last_byte ~first_line:line ~last_line:line

(* Warnings *)

let warn_unsupported_hard_break =
  "Hard breaks are unsupported in ocamlmark, using a soft break."

let warn_unsupported_header_nesting =
  "Headers in list items are unsupported in ocamlmark, dropped."

let warn_heading_level_6 =
  "Heading level 6 is unsupported in ocamlmark, using 5."

let warn_unsupported_list_start_number start =
  strf "List start numbers are unsupported in ocamlmark, replacing %d with 1."
    start

let warn_unsupported_cmark kind =
  strf "%s are unsupported in ocamlmark, dropped." kind

let warn_unsupported_link_title =
  "Link titles are unsupported in ocamlmark, dropped."

let warn ~loc:location message warns = { Warning.location; message } :: warns

let warn_unsupported_cmark ~locator kind meta (acc, warns) =
  let msg = warn_unsupported_cmark kind in
  (acc, warn ~loc:(meta_to_loc ~locator meta) msg warns)

let warn_unsupported_header_nesting ~locator meta (acc, warns) =
  let msg = warn_unsupported_header_nesting in
  (acc, warn ~loc:(meta_to_loc ~locator meta) msg warns)

let is_blank = function ' ' | '\t' -> true | _ -> false
let rec next_blank s ~max i =
  if i > max || is_blank s.[i] then i else next_blank s ~max (i + 1)

let rec next_nonblank s ~max i =
  if i > max || not (is_blank s.[i]) then i else next_nonblank s ~max (i + 1)

(* Translating blocks and inlines. *)

(* A few type definitions for better variant typing. *)

type inlines_acc = Ast.inline_element Ast.with_location list * Warning.t list
type ast_acc = Ast.t * Warning.t list
type nestable_ast_acc =
  Ast.nestable_block_element Ast.with_location list * Warning.t list

(* Inline translations *)

let link_definition defs l =
  match Inline.Link.reference_definition defs l with
  | Some (Link_definition.Def (ld, _)) -> Some ld
  | Some (Block.Footnote.Def (_, _)) -> None
  | Some _ -> assert false
  | None -> assert false (* assert [l]'s referenced label is not synthetic *)

let autolink_to_inline_element ~locator a m (is, warns) =
  let loc = meta_to_loc ~locator m in
  let link, link_loc = Inline.Autolink.link a in
  let link_loc = meta_to_loc ~locator link_loc in
  let text = [ Loc.at link_loc (`Word link) ] in
  (Loc.at loc (`Link (link, text)) :: is, warns)

let break_to_inline_element ~locator br m (is, warns) =
  let loc = meta_to_loc ~locator m in
  let warns =
    match Inline.Break.type' br with
    | `Soft -> warns
    | `Hard -> warn ~loc warn_unsupported_hard_break warns
  in
  (Loc.at loc (`Space "\n") :: is, warns)

let code_span_to_inline_element ~locator cs m (is, warns) =
  let loc = meta_to_loc ~locator m in
  let code = Inline.Code_span.code cs in
  (Loc.at loc (`Code_span code) :: is, warns)

let math_span_to_inline_element ~locator ms m (is, warns) =
  let loc = meta_to_loc ~locator m in
  let tex = Inline.Math_span.tex ms in
  (Loc.at loc (`Math_span tex) :: is, warns)

let raw_html_to_inline_element ~locator html m (is, warns) =
  let loc = meta_to_loc ~locator m in
  let html = String.concat "\n" (List.map Block_line.tight_to_string html) in
  (Loc.at loc (`Raw_markup (Some "html", html)) :: is, warns)

let image_to_inline_element ~locator defs i m (is, warns) =
  (* We map to raw html, ocamldoc's ast should have a case for that. *)
  let escape esc b s =
    Buffer.clear b;
    esc b s;
    Buffer.contents b
  in
  let pct_esc = escape Cmarkit_html.buffer_add_pct_encoded_string in
  let html_esc = escape Cmarkit_html.buffer_add_html_escaped_string in
  let loc = meta_to_loc ~locator m in
  let b = Buffer.create 255 in
  let ld = link_definition defs i in
  match ld with
  | None -> (is, warns)
  | Some ld ->
      let link =
        match Link_definition.dest ld with
        | None -> ""
        | Some (link, _) -> pct_esc b link
      in
      let title =
        match Link_definition.title ld with
        | None -> ""
        | Some title ->
            let title = List.map Block_line.tight_to_string title in
            html_esc b (String.concat "\n" title)
      in
      let alt =
        let ls =
          Inline.to_plain_text ~break_on_soft:false (Inline.Link.text i)
        in
        html_esc b (String.concat "\n" (List.map (String.concat "") ls))
      in
      let img =
        String.concat ""
          [
            {|<img src="|};
            link;
            {|" alt="|};
            alt;
            {|" title="|};
            title;
            {|" >"|};
          ]
      in
      (Loc.at loc (`Raw_markup (Some "html", img)) :: is, warns)

let text_to_inline_elements ~locator s meta ((is, warns) as acc) =
  (* [s] is on a single source line (but may have newlines because of
     character references) we need to tokenize it for ocamldoc's ast. *)
  let flush_tok s meta acc is_space first last =
    let textloc = textloc_of_sub (Meta.textloc meta) ~first ~last in
    let loc = textloc_to_loc ~locator textloc in
    let s = String.sub s first (last - first + 1) in
    Loc.at loc (if is_space then `Space s else `Word s) :: acc
  in
  let rec tokenize s meta acc max start is_space =
    if start > max then (List.rev_append acc is, warns)
    else
      let next_start =
        if is_space then next_nonblank s ~max start else next_blank s ~max start
      in
      let acc = flush_tok s meta acc is_space start (next_start - 1) in
      tokenize s meta acc max next_start (not is_space)
  in
  let max = String.length s - 1 in
  if max < 0 then acc else tokenize s meta [] max 0 (is_blank s.[0])

let rec link_reference_to_inline_element ~locator defs l m (is, warns) =
  let loc = meta_to_loc ~locator m in
  let ld = link_definition defs l in
  match ld with
  | None ->
      let text, warns =
        inline_to_inline_elements ~locator defs ([], warns) (Inline.Link.text l)
      in
      (text @ is, warns)
  | Some ld ->
      let replace_md_mdx s =
        let add_html x = x ^ ".html" in
        if String.ends_with ~suffix:".md" s then
          String.sub s 0 (String.length s - 3) |> add_html
        else if String.ends_with ~suffix:".mdx" s then
          String.sub s 0 (String.length s - 4) |> add_html
        else s
      in
      let link =
        match Link_definition.dest ld with
        | None -> ""
        | Some (l, _) ->
            if String.contains l ':' then (* Assume it's a URL *) l
            else
              (* If it ends with `.md` or `.mdx`, drop the extension and add `.html` *)
              replace_md_mdx l
      in
      let warns =
        match Link_definition.title ld with
        | None -> warns
        | Some title ->
            let textloc = Block_line.tight_list_textloc title in
            let loc = textloc_to_loc ~locator textloc in
            warn ~loc warn_unsupported_link_title warns
      in
      let text, warns =
        inline_to_inline_elements ~locator defs ([], warns) (Inline.Link.text l)
      in
      (Loc.at loc (`Link (link, text)) :: is, warns)

and link_to_inline_element ~locator defs l m acc =
  link_reference_to_inline_element ~locator defs l m acc

and emphasis_to_inline_element ~locator defs style e m (is, warns) =
  let loc = meta_to_loc ~locator m in
  let i = Inline.Emphasis.inline e in
  let inlines, warns = inline_to_inline_elements ~locator defs ([], warns) i in
  (Loc.at loc (`Styled (style, inlines)) :: is, warns)

and inline_to_inline_elements ~locator defs acc i : inlines_acc =
  match i with
  | Inline.Autolink (a, m) -> autolink_to_inline_element ~locator a m acc
  | Inline.Break (b, m) -> break_to_inline_element ~locator b m acc
  | Inline.Code_span (cs, m) -> code_span_to_inline_element ~locator cs m acc
  | Inline.Emphasis (e, m) ->
      emphasis_to_inline_element ~locator defs `Emphasis e m acc
  | Inline.Image (i, m) -> image_to_inline_element ~locator defs i m acc
  | Inline.Inlines (is, _m) ->
      let inline = inline_to_inline_elements ~locator defs in
      List.fold_left inline acc (List.rev is)
  | Inline.Link (l, m) -> link_to_inline_element ~locator defs l m acc
  | Inline.Raw_html (html, m) -> raw_html_to_inline_element ~locator html m acc
  | Inline.Strong_emphasis (e, m) ->
      emphasis_to_inline_element ~locator defs `Bold e m acc
  | Inline.Text (t, m) -> text_to_inline_elements ~locator t m acc
  | Inline.Ext_math_span (ms, m) ->
      math_span_to_inline_element ~locator ms m acc
  | Inline.Ext_strikethrough (s, meta) ->
      let i = Inline.Strikethrough.inline s in
      let acc = warn_unsupported_cmark ~locator "strikethrough" meta acc in
      inline_to_inline_elements ~locator defs acc i
  | _ -> assert false

(* Heading label support - CommonMark extension. Parses a potential
   final {#id} in heading inlines. In [id] braces must be escaped
   otherwise parsing fails; if the rightmost brace is escaped it's
   not a heading label. The parse runs from right to left *)

let parse_heading_label s =
  let rec loop s max prev i =
    if i < 0 then None
    else
      match s.[i] with
      | '{' as c ->
          if i > 0 && s.[i - 1] = '\\' then loop s max c (i - 1)
          else if prev = '#' then Some i
          else None
      | '}' as c ->
          if i > 0 && s.[i - 1] = '\\' then loop s max c (i - 1) else None
      | c -> loop s max c (i - 1)
  in
  let max = String.length s - 1 in
  let last =
    (* [last] is rightmost non blank, if any. *)
    let k = ref max in
    while (not (!k < 0)) && is_blank s.[!k] do
      decr k
    done;
    !k
  in
  if last < 1 || s.[last] <> '}' || s.[last - 1] = '\\' then None
  else
    match loop s max s.[last] (last - 1) with
    | None -> None
    | Some first ->
        let chop = max - first + 1 in
        let text = String.sub s 0 first in
        let first = first + 2 and last = last - 1 in
        (* remove delims *)
        let label = String.sub s first (last - first + 1) in
        Some (text, chop, label)

let heading_inline_and_label h =
  (* cmarkit claims it's already normalized but let's be defensive :-) *)
  match Inline.normalize (Block.Heading.inline h) with
  | Inline.Text (t, m) as inline -> (
      match parse_heading_label t with
      | None -> (inline, None)
      | Some (t, chop, label) ->
          let m = chop_end_of_meta_textloc ~count:chop m in
          (Inline.Text (t, m), Some label))
  | Inline.Inlines (is, m0) as inline -> (
      match List.rev is with
      | Inline.Text (t, m1) :: ris -> (
          match parse_heading_label t with
          | None -> (inline, None)
          | Some (t, chop, label) ->
              let m0 = chop_end_of_meta_textloc ~count:chop m0 in
              let m1 = chop_end_of_meta_textloc ~count:chop m1 in
              ( Inline.Inlines (List.rev (Inline.Text (t, m1) :: ris), m0),
                Some label ))
      | _ -> (inline, None))
  | inline -> (inline, None)

(* Block translations *)

let raw_paragraph ~loc ~raw_loc backend raw =
  Loc.at loc (`Paragraph [ Loc.at raw_loc (`Raw_markup (Some backend, raw)) ])

let code_block_to_nestable_block_element ~locator cb m (bs, warns) =
  let loc = meta_to_loc ~locator m in
  let code = Block.Code_block.code cb in
  let code_loc = textloc_to_loc ~locator (Block_line.list_textloc code) in
  let code = String.concat "\n" (List.map Block_line.to_string code) in
  match Block.Code_block.info_string cb with
  | None ->
      let code_block =
        {
          Ast.meta = None;
          delimiter = None;
          content = Loc.at code_loc code;
          output = None;
        }
        (* (None, Loc.at code_loc code) *)
      in
      (Loc.at loc (`Code_block code_block) :: bs, warns)
  | Some (info, im) -> (
      match Block.Code_block.language_of_info_string info with
      | None ->
          let code_block =
            {
              Ast.meta = None;
              delimiter = None;
              content = Loc.at code_loc code;
              output = None;
            }
          in
          (* (None, Loc.at code_loc code) *)
          (Loc.at loc (`Code_block code_block) :: bs, warns)
      | Some ("verb", _) -> (Loc.at loc (`Verbatim code) :: bs, warns)
      | Some ("=html", _) ->
          (raw_paragraph ~loc ~raw_loc:code_loc "html" code :: bs, warns)
      | Some ("=latex", _) ->
          (raw_paragraph ~loc ~raw_loc:code_loc "latex" code :: bs, warns)
      | Some ("=texi", _) ->
          (raw_paragraph ~loc ~raw_loc:code_loc "texi" code :: bs, warns)
      | Some ("=man", _) ->
          (raw_paragraph ~loc ~raw_loc:code_loc "man" code :: bs, warns)
      | Some (lang, env) ->
          let left_count = String.length lang in
          let right_count = String.length env in
          let lang_loc, env_loc =
            split_info_string_locs ~left_count ~right_count im
          in
          let env =
            if env = "" then []
            else [ `Tag (Loc.at (textloc_to_loc ~locator env_loc) env) ]
          in
          let lang = Loc.at (textloc_to_loc ~locator lang_loc) lang in
          let metadata = Some { Ast.language = lang; tags = env } in
          let code_block =
            {
              Ast.meta = metadata;
              delimiter = None;
              content = Loc.at code_loc code;
              output = None;
            }
            (* (metadata, Loc.at code_loc code) *)
          in
          (Loc.at loc (`Code_block code_block) :: bs, warns))

let math_block_to_nestable_block_element ~locator mb m (bs, warns) =
  let loc = meta_to_loc ~locator m in
  let math = Block.Code_block.code mb in
  let math = String.concat "\n" (List.map Block_line.to_string math) in
  (Loc.at loc (`Math_block math) :: bs, warns)

let html_block_to_nestable_block_element ~locator html m (bs, warns) =
  let loc = meta_to_loc ~locator m in
  let html = String.concat "\n" (List.map fst html) in
  (raw_paragraph ~loc ~raw_loc:loc "html" html :: bs, warns)

let heading_to_block_element ~locator defs h m (bs, warns) =
  let loc = meta_to_loc ~locator m in
  let level, warns =
    match Block.Heading.level h with
    | 6 -> (5, warn ~loc warn_heading_level_6 warns)
    | level -> (level, warns)
  in
  let inline, label = heading_inline_and_label h in
  let inlines, warns =
    inline_to_inline_elements ~locator defs ([], warns) inline
  in
  (Loc.at loc (`Heading (level, label, inlines)) :: bs, warns)

let paragraph_to_nestable_block_element ~locator defs p m (bs, warns) =
  (* TODO Parse inlines for @tags support. *)
  let loc = meta_to_loc ~locator m in
  let i = Block.Paragraph.inline p in
  let is, warns = inline_to_inline_elements ~locator defs ([], warns) i in
  (Loc.at loc (`Paragraph is) :: bs, warns)

let thematic_break_to_nestable_block_element ~locator m (bs, warns) =
  let loc = meta_to_loc ~locator m in
  (raw_paragraph ~loc ~raw_loc:loc "html" "<hr>" :: bs, warns)

let rec list_to_nestable_block_element ~locator defs l m (bs, warns) =
  let loc = meta_to_loc ~locator m in
  let style =
    `Heavy
    (* Note this is a layout property of ocamldoc *)
  in
  let kind, warns =
    match Block.List'.type' l with
    | `Unordered _ -> (`Unordered, warns)
    | `Ordered (start, _) ->
        ( `Ordered,
          if start = 1 then warns
          else warn ~loc (warn_unsupported_list_start_number start) warns )
  in
  let add_item ~locator (acc, warns) (i, _meta) =
    let b = Block.List_item.block i in
    let bs, warns =
      block_to_nestable_block_elements ~locator defs ([], warns) b
    in
    (bs :: acc, warns)
  in
  let ritems = List.rev (Block.List'.items l) in
  let items, warns = List.fold_left (add_item ~locator) ([], warns) ritems in
  (Loc.at loc (`List (kind, style, items)) :: bs, warns)

and table_to_nestable_block_element ~locator defs tbl m (bs, warns) =
  let loc = meta_to_loc ~locator m in
  let style =
    `Light
    (* Note this is a layout property of ocamldoc *)
  in
  let col_count = Block.Table.col_count tbl in
  let add_cell typ (n_cell, acc, warns) (cell, _) =
    let content, warns =
      inline_to_inline_elements ~locator defs ([], warns) cell
    in
    let loc = Loc.span (List.map Loc.location content) in
    let cell = Loc.at loc (`Paragraph content) in
    (n_cell + 1, ([ cell ], typ) :: acc, warns)
  in
  let add_cells (acc, warns) typ cells =
    let n_cell, res, warns =
      List.fold_left (add_cell typ) (0, [], warns) cells
    in
    let res =
      (* Pad with empty entries to reach the number of columns *)
      List.init (col_count - n_cell) (fun _ -> ([], `Data)) @ res |> List.rev
    in
    (res :: acc, warns)
  in
  let add_row ~locator:_ (acc, warns) (row, _meta) =
    match row with
    | `Header cells, _layout -> add_cells (acc, warns) `Header cells
    | `Data cells, _ -> add_cells (acc, warns) `Data cells
    | `Sep _, _ -> (acc, warns)
  in
  let rows = List.rev (Block.Table.rows tbl) in
  let items, warns = List.fold_left (add_row ~locator) ([], warns) rows in
  let alignment =
    let rec find_sep rows =
      match rows with
      | [] -> None
      | ((`Sep s, _layout), _meta) :: _ -> Some s
      | _ :: q -> find_sep q
    in
    match find_sep rows with
    | None -> None
    | Some sep -> Some (List.map (function (align, _layout), _ -> align) sep)
  in
  let table = `Table ((items, alignment), style) in
  let res = (Loc.at loc table :: bs, warns) in
  res

and block_to_nestable_block_elements ~locator defs acc b : nestable_ast_acc =
  match b with
  | Block.Blocks (bs, _) ->
      let block = block_to_nestable_block_elements ~locator defs in
      List.fold_left block acc (List.rev bs)
  | Block.Code_block (c, m) ->
      code_block_to_nestable_block_element ~locator c m acc
  | Block.Heading (_, m) -> warn_unsupported_header_nesting ~locator m acc
  | Block.Html_block (html, m) ->
      html_block_to_nestable_block_element ~locator html m acc
  | Block.List (l, m) -> list_to_nestable_block_element ~locator defs l m acc
  | Block.Paragraph (p, m) ->
      paragraph_to_nestable_block_element ~locator defs p m acc
  | Block.Block_quote (_, m) ->
      warn_unsupported_cmark ~locator "Block quotes" m acc
  | Block.Thematic_break (_, m) ->
      thematic_break_to_nestable_block_element ~locator m acc
  | Block.Blank_line _ | Block.Link_reference_definition _ ->
      (* layout cases *) acc
  | Block.Ext_table (tbl, m) ->
      table_to_nestable_block_element ~locator defs tbl m acc
  | Block.Ext_math_block (math, m) ->
      math_block_to_nestable_block_element ~locator math m acc
  | Block.Ext_footnote_definition (_, meta) ->
      warn_unsupported_cmark ~locator "Footnotes" meta acc
  | _ -> assert false

let rec block_to_ast ~locator defs acc b : ast_acc =
  match b with
  | Block.Heading (h, m) -> heading_to_block_element ~locator defs h m acc
  | Block.Blocks (bs, _) ->
      List.fold_left (block_to_ast ~locator defs) acc (List.rev bs)
  | b ->
      (* We can't go directy with acc because of nestable typing. *)
      let bs, ws = acc in
      let bs', ws = block_to_nestable_block_elements ~locator defs ([], ws) b in
      (List.rev_append (List.rev (bs' :> Ast.t)) bs, ws)

(* Parsing comments *)

let parse_comment ?buffer:b ~location ~text:s () : Ast.t * Warning.t list =
  let b =
    match b with
    | None -> Buffer.create (String.length s)
    | Some b ->
        Buffer.reset b;
        b
  in
  let locator, text = massage_comment ~location b s in
  let warns = ref [] and file = location.Lexing.pos_fname in
  let doc = Doc.of_string ~file ~locs:true ~strict:false text in
  block_to_ast ~locator (Doc.defs doc) ([], !warns) (Doc.block doc)
