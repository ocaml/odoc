open Odoc_document
open Types
open Doctree

module Markup = struct
  type t =
    | Concat of t list
    | Command of string * t
    | Env of indent * string * t * string
    | Space
    | Break
    | String of string
    | Indent of indent * int * t

  and indent = Any | V
  
  let noop = Concat []

  let sp = Space

  let break = Break

  let command s t = Command (s,t)
  let env indent s s' t = Env (indent,s,t,s')

  let append t1 t2 =
    match (t1, t2) with
    | Concat l1, Concat l2 -> Concat (l1 @ l2)
    | Concat l1, e2 -> Concat (l1 @ [ e2 ])
    | e1, Concat l2 -> Concat (e1 :: l2)
    | e1, e2 -> Concat [ e1; e2 ]

  let ( ++ ) = append

  let concat = List.fold_left ( ++ ) (Concat [])

  let rec intersperse ~sep = function
    | [] -> []
    | [ h ] -> [ h ]
    | h1 :: (_ :: _ as t) -> h1 :: sep :: intersperse ~sep t

  let list ?(sep = Concat []) l = concat @@ intersperse ~sep l

  let indent indent i content = Indent (indent, i, content)

  let str fmt = Format.ksprintf (fun s -> String s) fmt

  let escaped fmt = Format.ksprintf (fun s -> String s) fmt

  let rec pp ppf t = match t with
    | Concat l -> pp_many ppf l
    | String s -> Format.pp_print_string ppf s
    | Space -> Format.fprintf ppf "@ "
    | Break -> Format.fprintf ppf "@,"
    | Command (s, t) ->
        Format.fprintf ppf "@[{%s%a}@]"
          s pp t
    | Env (indent,s, t, s') ->
        Format.fprintf ppf "@[<v>@[<%s2>%s@ %a@]@,%s@]"
          (match indent with Any -> "" | V -> "v")
          s pp t s'
    | Indent (indent, i, content) ->
        Format.fprintf ppf "@[<%s%i>%a@]"
          (match indent with Any -> "" | V -> "v") i
          pp content
  and pp_many ppf l = List.iter (pp ppf) l
end

open Markup

let style (style : style) content =
  let s = match style with
    | `Bold -> "b "
    | `Italic -> "i "    
    | `Emphasis -> "e "
    | `Superscript -> "^ "
    | `Subscript -> "_ "
  in
  command s content

(* Striped content should be rendered in one line, without styling *)
let strip l =
  let rec loop acc = function
    | [] -> acc
    | h :: t -> (
        match h.Inline.desc with
        | Text _ | Entity _ | Raw_markup _ -> loop (h :: acc) t
        | Linebreak -> loop acc t
        | Styled (sty, content) ->
            let h =
              { h with desc = Styled (sty, List.rev @@ loop [] content) }
            in
            loop (h :: acc) t
        | Link (_, content)
        | InternalLink (Resolved (_, content))
        | InternalLink (Unresolved content) ->
            let acc = loop acc content in
            loop acc t
        | Source code ->
            let acc = loop_source acc code in
            loop acc t)
  and loop_source acc = function
    | [] -> acc
    | Source.Elt content :: t -> loop_source (List.rev_append content acc) t
    | Source.Tag (_, content) :: t ->
        let acc = loop_source acc content in
        loop_source acc t
  in
  List.rev @@ loop [] l

(* Partial support for now *)
let entity e =
  match e with "#45" -> escaped "-" | "gt" -> str ">" | s -> str "&%s;" s

(* Should hopefully make people notice and report *)

let raw_markup (_ : Raw_markup.t) = noop

let rec source_code (s : Source.t) =
  match s with
  | [] -> noop
  | h :: t -> (
      match h with
      | Source.Elt i -> inline (strip i) ++ source_code t
      | Tag (None, s) -> source_code s ++ source_code t
      | Tag (Some _, s) -> source_code s ++ source_code t)

and inline (l : Inline.t) =
  match l with
  | [] -> noop
  | i :: rest -> (
      match i.desc with
      | Text "" -> inline rest
      | Text _ ->
          let l, _, rest =
            Doctree.Take.until l ~classify:(function
              | { Inline.desc = Text s; _ } -> Accum [ s ]
              | _ -> Stop_and_keep)
          in
          str {|%s|} (String.concat "" l) ++ inline rest
      | Entity e ->
          let x = entity e in
          x ++ inline rest
      | Linebreak -> break ++ inline rest
      | Styled (sty, content) -> style sty (inline content) ++ inline rest
      | Link (href, content) ->
          command "" (str "{: %s}" href ++ inline (strip content))
          ++ inline rest
      | InternalLink (Resolved (_, content) | Unresolved content) ->
          command "!" (inline @@ strip content) ++ inline rest
      | Source content -> source_code content ++ inline rest
      | Raw_markup t -> raw_markup t ++ inline rest)

let rec block (l : Block.t) =
  match l with
  | [] -> noop
  | b :: rest -> (
      let continue r = if r = [] then noop else break ++ block r in
      match b.desc with
      | Inline i -> inline i ++ continue rest
      | Paragraph i -> inline i ++ continue rest
      | List (list_typ, l) ->
          let f n b =
            let bullet =
              match list_typ with
              | Unordered -> escaped "-"
              | Ordered -> str "%d)" (n + 1)
            in
            indent V 2 (bullet ++ sp ++ block b)
          in
          list ~sep:break (List.mapi f l) ++ continue rest
      | Description _ ->
          let descrs, _, rest =
            Take.until l ~classify:(function
              | { Block.desc = Description l; _ } -> Accum l
              | _ -> Stop_and_keep)
          in
          let f i =
            let key = inline i.Description.key in
            let def = block i.Description.definition in
            indent V 2 (str "@" ++ key ++ str ":" ++ sp ++ def)
          in
          list ~sep:break (List.map f descrs) ++ continue rest
      | Source content ->
          env Any "{[" "]}"  (source_code content) ++ continue rest
      | Verbatim content ->
          env Any "{v" "v}" (str "%s" content) ++ continue rest
      | Raw_markup t -> raw_markup t ++ continue rest)

let heading { Heading.label; level; title } =
  let level = string_of_int level in
  let title = inline title in
  env Any "(**" "*)" @@ match label with
  | Some label -> 
      command level (str ":%s " label ++ title)
  | None -> 
      command (level ^ " ") (title)

let expansion_not_inlined url = not (Link.should_inline url)

let take_code l =
  let c, _, rest =
    Take.until l ~classify:(function
      | DocumentedSrc.Code c -> Accum c
      | DocumentedSrc.Alternative (Expansion e) when expansion_not_inlined e.url
        ->
          Accum e.summary
      | _ -> Stop_and_keep)
  in
  (c, rest)

let inline_subpage = function
  | `Inline | `Open | `Default -> true
  | `Closed -> false

let rec documentedSrc (l : DocumentedSrc.t) =
  match l with
  | [] -> noop
  | line :: rest -> (
      let continue r = documentedSrc r in
      match line with
      | Code _ ->
          let c, rest = take_code l in
          source_code c ++ continue rest
      | Alternative alt -> (
          match alt with
          | Expansion { expansion; url; _ } ->
              if expansion_not_inlined url then
                let c, rest = take_code l in
                source_code c ++ continue rest
              else documentedSrc expansion)
      | Subpage p ->
          subpage p.content ++ continue rest
      | Documented _ | Nested _ ->
          let lines, _, rest =
            Take.until l ~classify:(function
              | DocumentedSrc.Documented { code; doc; _ } ->
                  Accum [ (`D code, doc) ]
              | DocumentedSrc.Nested { code; doc; _ } ->
                  Accum [ (`N code, doc) ]
              | _ -> Stop_and_keep)
          in
          let f (content, doc) =
            let doc =
              match doc with
              | [] -> noop
              | doc -> break ++ env V "(**" "*)" (block doc)
            in
            let content =
              match content with
              | `D code -> inline code
              | `N l -> documentedSrc l
            in
            break ++ content ++ doc
          in
          let l = list ~sep:noop (List.map f lines) in
          l ++ continue rest)

and subpage { title = _; header = _; items; url = _ } =
  let content = items in
  let surround body =
    if content = [] then sp
    else break ++ break ++ body ++ break
  in
  surround @@ item content

and item (l : Item.t list) =
  match l with
  | [] -> noop
  | i :: rest -> (
      let continue r = if r = [] then noop else break ++ break ++ item r in
      match i with
      | Text b ->
          let d = env Any "(**" "*)" (block b) in
          d ++ continue rest
      | Heading h ->
          heading h ++ continue rest
      | Declaration { attr = _; anchor = _; content; doc } ->
          let decl = documentedSrc content in
          let doc =
            match doc with
            | [] -> noop
            | doc -> env V "(**" "*)" (block doc) ++ break
          in
          doc ++ indent V 2 decl ++ continue rest
      | Include
          { attr = _; anchor = _; content = { summary; status; content }; doc }
        ->
          let d =
            if inline_subpage status then item content
            else
              let s = source_code summary in
              match doc with
              | [] -> s
              | doc -> s ++ break ++ env Any "(**" "*)" (block doc)
          in
          d ++ continue rest)

let on_sub subp =
  match subp with
  | `Page p -> if Link.should_inline p.Subpage.content.url then Some 1 else None
  | `Include incl -> if inline_subpage incl.Include.status then Some 0 else None

let page { Page.title = _ ; header; items = i; url = _ } =
  let header = Shift.compute ~on_sub header in
  let i = Shift.compute ~on_sub i in
  indent V 0 (
    item header
    ++ break ++ break
    ++ item i
  )

let rec subpage subp =
  let p = subp.Subpage.content in
  if Link.should_inline p.url then [] else [ render p ]

and render (p : Page.t) =
  let content ppf = Format.fprintf ppf "%a@." Markup.pp (page p) in
  let children = Utils.flatmap ~f:subpage @@ Subpages.compute p in
  let filename = Link.as_filename p.url in
  { Renderer.filename; content; children }
