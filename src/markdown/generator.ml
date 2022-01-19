open Odoc_document
open Types
open Doctree
open Link
open Markup

(** Make a new string by copying the given string [n] times. *)
let string_repeat n s =
  let s_len = String.length s in
  let b = Bytes.create (s_len * n) in
  for i = 0 to n - 1 do
    Bytes.unsafe_blit_string s 0 b (i * s_len) s_len
  done;
  Bytes.unsafe_to_string b

let style (style : style) =
  match style with
  | `Bold -> bold
  | `Italic | `Emphasis -> italic
  | `Superscript -> superscript
  | `Subscript -> subscript

type args = { generate_links : bool }

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

let rec source_code (s : Source.t) args =
  match s with
  | [] -> noop
  | h :: t -> (
      let continue s =
        if source_contains_text s then source_code s args else noop
      in
      match h with
      | Source.Elt i -> inline i args ++ continue t
      | Tag (Some "arrow", _) ->
          text "->" (* takes care of the Entity branch of Inline.t *)
      | Tag (_, s) -> continue s ++ continue t)

and inline (l : Inline.t) args =
  match l with
  | [] -> noop
  | i :: rest -> (
      let continue i = if i = [] then noop else inline i args in
      match i.desc with
      | Text "" | Text " " -> continue rest
      | Text _ ->
          let l, _, rest =
            Doctree.Take.until l ~classify:(function
              | { Inline.desc = Text s; _ } -> Accum [ s ]
              | _ -> Stop_and_keep)
          in
          text String.(concat "" l |> trim) ++ continue rest
      | Entity _ -> noop
      | Styled (styl, content) -> style styl (continue content) ++ continue rest
      | Linebreak -> line_break ++ continue rest
      | Link (href, content) ->
          link ~href (inline content args) ++ continue rest
      | InternalLink (Resolved (link', content)) ->
          if args.generate_links then
            match link'.page.parent with
            | Some _ -> continue content ++ continue rest
            | None ->
                link ~href:("#" ^ link'.anchor) (inline content args)
                ++ continue rest
          else continue content ++ continue rest
      | InternalLink (Unresolved content) -> continue content ++ continue rest
      | Source content -> source_code content args ++ continue rest
      | Raw_markup (_, s) -> text s ++ continue rest)

let rec blocks' (bs : blocks list) =
  match bs with
  | [] -> paragraph noop
  | [ b ] -> b
  | b :: rest -> blocks b (blocks' rest)

let rec block (l : Block.t) args =
  let noop = paragraph noop in
  match l with
  | [] -> noop
  | b :: rest -> (
      let continue r = if r = [] then noop else block r args in
      match b.desc with
      | Inline i -> blocks (paragraph (inline i args)) (continue rest)
      | Paragraph i -> blocks (paragraph (inline i args)) (continue rest)
      | List (list_typ, l') ->
          let f bs =
            match list_typ with
            | Unordered -> unordered_list bs
            | Ordered -> ordered_list bs
          in
          blocks (f (List.map (fun b -> block b args) l')) (continue rest)
      | Description _ ->
          let descrs, _, rest =
            Take.until l ~classify:(function
              | { Block.desc = Description l; _ } -> Accum l
              | _ -> Stop_and_keep)
          in
          let f i =
            let key = inline i.Description.key args in
            let def =
              match i.Description.definition with
              | [] -> text ""
              | h :: _ -> (
                  match h.desc with Inline i -> inline i args | _ -> text "")
            in
            paragraph (join (text "@") (join key (text ":")) ++ def)
          in
          blocks (blocks' (List.map f descrs)) (continue rest)
      | Source content ->
          blocks (paragraph (source_code content args)) (continue rest)
      | Verbatim content -> blocks (code_block content) (continue rest)
      | Raw_markup (_, s) -> blocks (raw_markup s) (continue rest))

let rec acc_text (l : Block.t) : string =
  match l with
  | [] -> ""
  | h :: rest -> (
      match h.desc with Paragraph i -> inline_text i ^ acc_text rest | _ -> "")

and inline_text (i : Inline.t) =
  match i with
  | [] -> ""
  | h :: rest -> (
      match h.desc with
      | Text s -> s ^ inline_text rest
      | Source s ->
          let rec source_text (s' : Source.t) =
            match s' with
            | [] -> ""
            | t :: rest_t -> (
                match t with
                | Elt i -> inline_text i ^ source_text rest_t
                | _ -> "")
          in
          code_span (source_text s)
      | _ -> "")

(** Generates the 6-heading used to differentiate items. Non-breaking spaces
    are inserted just before the text, to simulate indentation depending on
    [nesting_level].
    {v
      ######<space><nbsps><space>Text
    v} *)
let item_heading nesting_level content =
  let pre_hash = text (String.make 6 '#')
  and pre_nbsp =
    if nesting_level = 0 then noop
    else text (string_repeat (nesting_level * 2) "\u{A0}")
  in
  paragraph (pre_hash ++ pre_nbsp ++ content)

let rec documented_src (l : DocumentedSrc.t) args nesting_level =
  let noop = paragraph noop in
  let take_code l =
    let c, _, rest =
      Take.until l ~classify:(function
        | DocumentedSrc.Code c -> Accum c
        | _ -> Stop_and_keep)
    in
    (c, rest)
  in
  match l with
  | [] -> noop
  | line :: rest -> (
      let continue r =
        if r = [] then noop else documented_src r args nesting_level
      in
      match line with
      | Code s ->
          if source_contains_text s then
            let c, rest = take_code l in
            blocks
              (item_heading nesting_level (source_code c args))
              (continue rest)
          else noop
      | Alternative (Expansion { url; expansion; _ }) ->
          if should_inline url then documented_src expansion args nesting_level
          else continue rest
      | Subpage p ->
          blocks (subpage p.content args (nesting_level + 1)) (continue rest)
      | Documented _ | Nested _ ->
          let lines, _, rest =
            Take.until l ~classify:(function
              | DocumentedSrc.Documented { code; doc; anchor; _ } ->
                  Accum [ (`D code, doc, anchor) ]
              | DocumentedSrc.Nested { code; doc; anchor; _ } ->
                  Accum [ (`N code, doc, anchor) ]
              | _ -> Stop_and_keep)
          in
          let f (content, doc, (anchor : Odoc_document.Url.t option)) =
            let doc =
              match doc with
              | [] -> noop
              | doc -> paragraph (text (acc_text doc))
            in
            let content =
              let nesting_level = nesting_level + 1 in
              match content with
              | `D code (* for record fields and polymorphic variants *) ->
                  item_heading nesting_level (inline code args)
              | `N l (* for constructors *) ->
                  let c, rest = take_code l in
                  blocks
                    (item_heading nesting_level (source_code c args))
                    (continue rest)
            in
            let item = blocks content doc in
            if args.generate_links then
              let anchor =
                match anchor with Some a -> a.anchor | None -> ""
              in
              blocks (paragraph (anchor' anchor)) item
            else item
          in
          blocks (blocks' (List.map f lines)) (continue rest))

and subpage { title = _; header = _; items; url = _ } args nesting_level =
  let content = items in
  let subpage' body = if content = [] then paragraph line_break else body in
  subpage' @@ item content args nesting_level

and item (l : Item.t list) args nesting_level =
  let noop = paragraph noop in
  match l with
  | [] -> noop
  | i :: rest -> (
      let continue r = if r = [] then noop else item r args nesting_level in
      match i with
      | Text b -> blocks (block b args) (continue rest)
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
      | Declaration { attr = _; anchor; content; doc } ->
          let decl = documented_src content args nesting_level in
          let doc =
            match doc with [] -> noop | doc -> paragraph (text (acc_text doc))
          in
          let item' = blocks decl doc in
          if args.generate_links then
            let anchor = match anchor with Some x -> x.anchor | None -> "" in
            blocks (blocks (paragraph (anchor' anchor)) item') (continue rest)
          else blocks item' (continue rest)
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

let page { Page.header; items; url; _ } args =
  let blocks'' l = List.map (fun s -> paragraph (text s)) l |> blocks' in
  blocks'
    ([ blocks'' (for_printing url) ]
    @ [ blocks (item header args 0) (item items args 0) ])

let rec subpage subp (args : args) =
  let p = subp.Subpage.content in
  if should_inline p.url then [] else [ render p args ]

and render (p : Page.t) args =
  let content fmt = Format.fprintf fmt "%a" pp_blocks (page p args) in
  let children =
    Utils.flatmap ~f:(fun sp -> subpage sp args) (Subpages.compute p)
  in
  let filename = as_filename p.url in
  { Odoc_document.Renderer.filename; content; children }
