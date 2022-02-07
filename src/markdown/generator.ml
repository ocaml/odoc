open Odoc_document
open Types
open Doctree
open Markup

(** Make a new string by copying the given string [n] times. *)
let string_repeat n s =
  let s_len = String.length s in
  let b = Bytes.create (s_len * n) in
  for i = 0 to n - 1 do
    Bytes.unsafe_blit_string s 0 b (i * s_len) s_len
  done;
  Bytes.unsafe_to_string b

(** Like [String.index_from_opt] but check against a predicate function. *)
let rec string_index_f f s i =
  if i >= String.length s then None
  else if f s.[i] then Some i
  else string_index_f f s (i + 1)

(** Remove spaces at the end of a string. *)
let string_trim_right s =
  let right = String.length s - 1 in
  let i = ref right in
  while !i >= 0 && s.[!i] = ' ' do
    decr i
  done;
  if !i = right then s else String.sub s 0 (!i + 1)

let style (style : style) =
  match style with
  | `Bold -> bold
  | `Italic | `Emphasis -> italic
  | `Superscript -> superscript
  | `Subscript -> subscript

let fold_inlines f elts : inlines =
  List.fold_left (fun acc elt -> acc ++ f elt) noop elts

let fold_blocks f elts : blocks =
  List.fold_left (fun acc elt -> acc +++ f elt) noop_block elts

type args = { base_path : Url.Path.t; generate_links : bool }

let rec source_contains_text (s : Source.t) =
  let inline_contains_text (i : Inline.t) =
    let check_inline_desc (i : Inline.desc) =
      match i with Text "" | Text " " -> false | Text _ | _ -> true
    in
    List.exists (fun { Inline.desc = d; _ } -> check_inline_desc d) i
  in
  let check_source (s : Source.token) =
    match s with
    | Source.Elt i -> inline_contains_text i
    | Tag (_, s) -> source_contains_text s
  in
  List.exists check_source s

let rec source_contains_only_text s =
  let check_inline i = match i.Inline.desc with Text _ -> true | _ -> false in
  let check_source = function
    | Source.Elt i -> List.for_all check_inline i
    | Tag (_, s) -> source_contains_only_text s
  in
  List.for_all check_source s

(** Split source code at the first [:] or [=]. *)
let source_take_until_punctuation code =
  let rec is_punctuation s i =
    if i >= String.length s then false
    else
      match s.[i] with
      | ' ' -> is_punctuation s (i + 1)
      | ':' | '=' -> true
      | _ -> false
  in
  let rec inline_take_until_punctuation acc = function
    | ({ Inline.desc = Text s; _ } as inline) :: tl when is_punctuation s 0 ->
        let inline = { inline with desc = Text (string_trim_right s) } in
        Some (List.rev (inline :: acc), tl)
    | hd :: tl -> inline_take_until_punctuation (hd :: acc) tl
    | [] -> None
  in
  let left, middle, right =
    Take.until code ~classify:(function
      | Source.Elt i as t -> (
          match inline_take_until_punctuation [] i with
          | Some (i, tl) -> Stop_and_accum ([ Source.Elt i ], Some tl)
          | None -> Accum [ t ])
      | Tag (_, c) -> Rec c)
  in
  let right =
    match middle with Some i -> Source.Elt i :: right | None -> right
  in
  (left, right)

let is_not_whitespace = function ' ' -> false | _ -> true

let rec inline_trim_begin = function
  | ({ Inline.desc = Text s; _ } as inline) :: tl -> (
      match string_index_f is_not_whitespace s 0 with
      | None -> inline_trim_begin tl
      | Some i ->
          let s = String.sub s i (String.length s - i) in
          { inline with desc = Text s } :: tl)
  | x -> x

(** Remove the spaces at the beginning of source code. *)
let rec source_trim_begin = function
  | Source.Elt i :: tl -> (
      match inline_trim_begin i with
      | [] -> source_trim_begin tl
      | i -> Source.Elt i :: tl)
  | Tag (attr, c) :: tl -> (
      match source_trim_begin c with
      | [] -> source_trim_begin tl
      | c -> Tag (attr, c) :: tl)
  | [] -> []

(** Used for code spans. Must be called only on sources that pass
    [source_contains_only_text s]. *)
let source_code_to_string s =
  let inline acc i =
    match i.Inline.desc with Text s -> s :: acc | _ -> assert false
  in
  let rec source_code s =
    List.fold_left
      (fun acc -> function
        | Source.Elt i -> List.fold_left inline acc i
        | Tag (_, t) -> List.rev_append (source_code t) acc)
      [] s
  in
  String.concat "" (List.rev (source_code s))

let rec source_code (s : Source.t) args = fold_inlines (source_code_one args) s

and source_code_one args = function
  | Source.Elt i -> inline i args
  | Tag (Some "arrow", _) -> text "->"
  | Tag (_, s) -> source_code s args

and inline l args = fold_inlines (inline_one args) l

and inline_one args i =
  match i.Inline.desc with
  | Text " " -> space
  | Text s -> text s
  | Entity _ -> noop
  | Styled (styl, content) -> style styl (inline content args)
  | Linebreak -> line_break
  | Link (href, content) -> link ~href (inline content args)
  | InternalLink (Resolved (url, content)) ->
      if args.generate_links then
        link
          ~href:(Link.href ~base_path:args.base_path url)
          (inline content args)
      else inline content args
  | InternalLink (Unresolved content) -> inline content args
  | Source content when source_contains_only_text content ->
      code_span (source_code_to_string content)
  | Source content -> source_code content args
  | Raw_markup (_, s) -> text s

let rec block args l = fold_blocks (block_one args) l

and block_one args b =
  match b.Block.desc with
  | Inline i -> paragraph (inline i args)
  | Paragraph i -> paragraph (inline i args)
  | List (list_typ, items) -> (
      let items = List.map (block args) items in
      match list_typ with
      | Unordered -> unordered_list items
      | Ordered -> ordered_list items)
  | Description l -> description args l
  | Source content -> code_block (source_code content args)
  | Verbatim content -> code_block (text content)
  | Raw_markup (_, s) -> raw_markup s

and description args l = fold_blocks (description_one args) l

and description_one args { Description.key; definition; _ } =
  let key = inline key args in
  let def =
    match definition with
    | [] -> noop
    | h :: _ -> (
        match h.desc with Inline i -> space ++ inline i args | _ -> noop)
  in
  paragraph (text "@" ++ key ++ def)

(** Generates the 6-heading used to differentiate items. Non-breaking spaces
    are inserted just before the text, to simulate indentation depending on
    [nesting_level].
    {v
      ######<space><nbsps><space>Text
    v} *)
let item_heading nesting_level content =
  let pre_nbsp =
    if nesting_level = 0 then noop
    else text (string_repeat (nesting_level * 2) "\u{A0}") ++ text " "
    (* Use literal spaces to avoid breaking. *)
  in
  heading 6 (pre_nbsp ++ content)

let take_code l =
  let c, _, rest =
    Take.until l ~classify:(function
      | DocumentedSrc.Code c -> Accum c
      | _ -> Stop_and_keep)
  in
  (c, rest)

let rec documented_src (l : DocumentedSrc.t) args nesting_level =
  match l with
  | [] -> noop_block
  | line :: rest -> (
      let continue r = documented_src r args nesting_level in
      match line with
      | Code s ->
          if source_contains_text s then
            let c, rest = take_code l in
            paragraph (source_code c args) +++ continue rest
          else continue rest
      | Alternative (Expansion { url; expansion; _ }) ->
          if Link.should_inline url then
            documented_src expansion args nesting_level +++ continue rest
          else continue rest
      | Subpage { content = { title = _; header = _; items; url = _ }; _ } ->
          let content =
            if items = [] then paragraph line_break
            else item items args (nesting_level + 1)
          in
          content +++ continue rest
      | Documented { code; doc; anchor; _ } ->
          documented args nesting_level (`D code) doc anchor +++ continue rest
      | Nested { code; doc; anchor; _ } ->
          documented args nesting_level (`N code) doc anchor +++ continue rest)

and documented args nesting_level content doc anchor =
  let content =
    let nesting_level = nesting_level + 1 in
    match content with
    | `D code (* for record fields and polymorphic variants *) ->
        item_heading nesting_level (inline code args)
    | `N l (* for constructors *) ->
        let c, rest = take_code l in
        item_heading nesting_level (source_code c args)
        +++ documented_src rest args nesting_level
  in
  let item = blocks content (block args doc) in
  if args.generate_links then
    let anchor =
      match anchor with Some a -> a.Url.Anchor.anchor | None -> ""
    in
    blocks (paragraph (anchor' anchor)) item
  else item

and item (l : Item.t list) args nesting_level =
  match l with
  | [] -> noop_block
  | i :: rest -> (
      let continue r = item r args nesting_level in
      match i with
      | Text b -> blocks (block args b) (continue rest)
      | Heading { Heading.label; level; title } ->
          let heading' =
            let title = inline title args in
            match label with
            | Some _ -> (
                match level with
                | 1 -> heading level title
                | _ -> blocks (heading level title) block_separator)
            | None -> paragraph title
          in
          blocks heading' (continue rest)
      | Declaration { attr = _; anchor; content; doc } -> (
          (*
             Declarations render like this:

             {v
             <a id="<id>"></a>
             ###### <nesting_level> <code from content>

             <rest of content, possibly big>

             <doc>
             v}
          *)
          let take_code_from_declaration content =
            match take_code content with
            | begin_code, Alternative (Expansion e) :: tl
              when Link.should_inline e.url ->
                (* Take the code from inlined expansion. For example, to catch
                   [= sig]. *)
                let e_code, e_tl = take_code e.expansion in
                (begin_code @ e_code, e_tl @ tl)
            | begin_code, content -> (begin_code, content)
          in
          let render_declaration ~anchor ~doc heading content =
            let anchor =
              if args.generate_links then
                let anchor =
                  match anchor with Some x -> x.Url.Anchor.anchor | None -> ""
                in
                paragraph (anchor' anchor)
              else noop_block
            in
            anchor
            +++ item_heading nesting_level (source_code heading args)
            +++ content +++ block args doc +++ continue rest
          in
          match take_code_from_declaration content with
          | code, [] ->
              (* Declaration is only code, render formatted code. *)
              let code, content = source_take_until_punctuation code in
              let content =
                match source_trim_begin content with
                | [] -> noop_block
                | content -> quote_block (paragraph (source_code content args))
              in
              render_declaration ~anchor ~doc code content
          | code, content ->
              render_declaration ~anchor ~doc code
                (documented_src content args nesting_level))
      | Include { content = { summary; status; content }; _ } ->
          let inline_subpage = function
            | `Inline | `Open | `Default -> true
            | `Closed -> false
          in
          let d =
            if inline_subpage status then item content args nesting_level
            else paragraph (source_code summary args)
          in
          blocks d (continue rest))

let page ~generate_links { Page.header; items; url; _ } =
  let args = { base_path = url; generate_links } in
  fold_blocks (fun s -> paragraph (text s)) (Link.for_printing url)
  +++ item header args 0 +++ item items args 0

let rec subpage ~generate_links subp =
  let p = subp.Subpage.content in
  if Link.should_inline p.url then [] else [ render ~generate_links p ]

and render ~generate_links (p : Page.t) =
  let content fmt =
    Format.fprintf fmt "%a" pp_blocks (page ~generate_links p)
  in
  let children =
    Utils.flatmap ~f:(fun sp -> subpage ~generate_links sp) (Subpages.compute p)
  in
  let filename = Link.as_filename p.url in
  { Odoc_document.Renderer.filename; content; children }
