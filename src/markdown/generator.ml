open Odoc_document
open Types
open Doctree

module Markup : sig
  type t

  val noop : t

  val break : t

  val nbsp : t

  val space : t

  val backticks : t

  val open_sq_bracket : t

  val close_sq_bracket : t

  val ( ++ ) : t -> t -> t

  val concat : t list -> t

  val inline' : string list -> t

  val block' : t list -> t

  val list : ?sep:t -> t list -> t

  val anchor' : string -> t

  val string : string -> t

  val str : ('a, unit, string, t) format4 -> 'a

  val escaped : ('a, unit, string, t) format4 -> 'a

  val open_parenthesis : t

  val close_parenthesis : t

  val pp : Format.formatter -> t -> unit
end = struct
  type t =
    | Block of t list
    | Concat of t list
    | Break
    | Space
    | Anchor of string
    | String of string
    | Backticks
    | Nbsp
    | OpenSqBracket
    | CloseSqBracket
    | OpenParenthesis
    | CloseParenthesis

  let noop = Concat []

  let break = Break

  let nbsp = Nbsp

  let space = Space

  let backticks = Backticks

  let open_sq_bracket, close_sq_bracket = (OpenSqBracket, CloseSqBracket)

  let open_parenthesis, close_parenthesis = (OpenParenthesis, CloseParenthesis)

  let append t1 t2 =
    match (t1, t2) with
    | Concat l1, Concat l2 -> Concat (l1 @ l2)
    | Concat l1, e2 -> Concat (l1 @ [ e2 ])
    | e1, Concat l2 -> Concat (e1 :: l2)
    | e1, e2 -> Concat [ e1; e2 ]

  let ( ++ ) = append

  let concat ts = Concat ts

  let rec intersperse ~sep = function
    | [] -> []
    | [ h ] -> [ h ]
    | h1 :: (_ :: _ as t) -> h1 :: sep :: intersperse ~sep t

  let list ?(sep = Concat []) l = concat @@ intersperse ~sep l

  let anchor' s = Anchor s

  let string s = String s

  let block' ts = Block ts

  let inline' l = List.map (fun s -> string s) l |> concat

  let str fmt = Format.ksprintf (fun s -> string s) fmt

  let escaped fmt = Format.ksprintf (fun s -> string s) fmt

  let rec pp fmt t =
    match t with
    | Block b ->
        let inner = function
          | [] -> ()
          | [ x ] -> Format.fprintf fmt "%a" pp x
          | x :: xs -> Format.fprintf fmt "%a@\n%a" pp x pp (Block xs)
        in
        inner b
    | Concat l -> pp_many fmt l
    | Break -> Format.fprintf fmt "@\n"
    | Space -> Format.fprintf fmt " "
    | Anchor s -> Format.fprintf fmt "<a id=\"%s\"></a>" s
    | String s -> Format.fprintf fmt "%s" s
    (* We use double backticks to take care of polymorphic variants or content
       within backtick, and the spaces before and after the backticks for
       clarity on what should be enclosed in backticks. For example,
       "type nums = [ | `One | `Two ]" would be rendered as "``|`````Monday`` "
       if the spaces were missing.
    *)
    | Backticks -> Format.fprintf fmt " `` "
    | Nbsp -> Format.fprintf fmt "\u{00A0}"
    | OpenSqBracket -> Format.fprintf fmt "["
    | CloseSqBracket -> Format.fprintf fmt "]"
    | OpenParenthesis -> Format.fprintf fmt "("
    | CloseParenthesis -> Format.fprintf fmt ")"

  and pp_many fmt l = List.iter (pp fmt) l
end

open Markup

let entity e =
  match e with "#45" -> escaped "-" | "gt" -> str ">" | s -> str "&%s;" s

let raw_markup (_ : Raw_markup.t) = noop

let style (style : style) content =
  match style with
  | `Bold -> string "**" ++ (content ++ str "**")
  | `Italic | `Emphasis -> string "_" ++ (content ++ str "_")
  | `Superscript -> string "<sup>" ++ content
  | `Subscript -> string "<sub>" ++ content

let make_hashes n = String.make n '#'

type args = { generate_links : bool ref; md_flavour : string ref }

let args = { generate_links = ref true; md_flavour = ref "" }

let rec source_code (s : Source.t) nbsp =
  match s with
  | [] -> noop
  | h :: t -> (
      let continue s = if s = [] then concat [] else source_code s nbsp in
      match h with
      | Source.Elt i -> inline i nbsp ++ continue t
      | Tag (None, s) -> continue s ++ continue t
      | Tag (Some _, s) -> continue s ++ continue t)

and inline (l : Inline.t) nbsp =
  match l with
  | [] -> noop
  | i :: rest -> (
      let continue i = if i = [] then noop else inline i nbsp in
      let make_link c s =
        open_sq_bracket ++ continue c ++ close_sq_bracket ++ open_parenthesis
        ++ string s ++ close_parenthesis ++ continue rest
      in
      let cond then_clause else_clause =
        if !(args.generate_links) then then_clause else else_clause
      in
      match i.desc with
      | Text "" -> continue rest
      | Text s -> (
          match s with
          | "end" | "}" | "]" ->
              string (make_hashes 6) ++ space ++ nbsp ++ string s
          | _ ->
              let l, _, rest =
                Doctree.Take.until l ~classify:(function
                  | { Inline.desc = Text s; _ } -> Accum [ str "%s" s ]
                  | _ -> Stop_and_keep)
              in
              concat l ++ continue rest)
      | Entity e ->
          let x = entity e in
          x ++ continue rest
      | Styled (sty, content) -> style sty (continue content) ++ continue rest
      | Linebreak -> break ++ continue rest
      | Link (href, content) ->
          cond
            (match content with
            | [] -> noop
            | i :: rest ->
                (match i.desc with
                | Text _ -> make_link content href
                | _ -> continue content ++ continue rest)
                ++ continue rest)
            (continue content ++ continue rest)
      | InternalLink (Resolved (link, content)) ->
          cond
            (match link.page.parent with
            | Some _ -> continue content ++ continue rest
            | None -> make_link content (make_hashes 1 ^ link.anchor))
            (continue content ++ continue rest)
      | InternalLink (Unresolved content) -> continue content ++ continue rest
      | Source content ->
          cond
            (source_code content nbsp ++ continue rest)
            (backticks ++ source_code content nbsp ++ backticks ++ continue rest)
      | Raw_markup t -> raw_markup t ++ continue rest)

let rec block (l : Block.t) nbsp =
  match l with
  | [] -> noop
  | b :: rest -> (
      let continue r = if r = [] then noop else block r nbsp in
      match b.desc with
      | Inline i -> inline i nbsp ++ continue rest
      | Paragraph i -> inline i nbsp ++ break ++ continue rest
      | List (list_typ, l) ->
          let f n b =
            let bullet =
              match list_typ with
              | Unordered -> escaped "- "
              | Ordered -> str "%d. " (n + 1)
            in
            bullet ++ block b nbsp ++ break
          in
          list ~sep:break (List.mapi f l) ++ continue rest
      | Description _ ->
          let descrs, _, rest =
            Take.until l ~classify:(function
              | { Block.desc = Description l; _ } -> Accum l
              | _ -> Stop_and_keep)
          in
          let f i =
            let key = inline i.Description.key nbsp in
            let def = block i.Description.definition nbsp in
            break ++ str "@" ++ key ++ str " : " ++ def ++ break ++ break
          in
          list ~sep:break (List.map f descrs) ++ continue rest
      | Source content -> source_code content nbsp ++ continue rest
      | Verbatim content ->
          space ++ space ++ space ++ space ++ str "%s" content ++ continue rest
      | Raw_markup t -> raw_markup t ++ continue rest)

let heading { Heading.label; level; title } nbsp =
  let title = inline title nbsp in
  let level =
    match level with
    | 1 -> make_hashes 1
    | 2 -> make_hashes 2
    | 3 -> make_hashes 3
    | 4 -> make_hashes 4
    | 5 -> make_hashes 5
    | 6 -> make_hashes 6
    | _ -> ""
    (* We can be sure that h6 will never be exceded! *)
  in
  match label with
  | Some label -> (
      let label = str " {#%s}" label in
      (* `---` forms a horizontal line below heading, except level one headings (h1)*)
      let sep = str "---" in
      let heading' level = string level ++ space ++ title in
      let cond then_clause else_clause =
        if !(args.md_flavour) = "pandoc" then then_clause else else_clause
      in
      match level with
      (* This match forms a horizontal line below the heading (for readability reasons),
         however, we ignore `h1` heading because by default a line is formed below it. *)
      | "#" -> cond (heading' level ++ label) (heading' level)
      | _ ->
          cond
            (heading' level ++ label ++ break ++ sep)
            (heading' level ++ break ++ sep))
  | None -> string level ++ title

let inline_subpage = function
  | `Inline | `Open | `Default -> true
  | `Closed -> false

let item_prop nbsp = string (make_hashes 6) ++ space ++ nbsp

let rec documented_src (l : DocumentedSrc.t) nbsp nbsp' =
  match l with
  | [] -> noop
  | line :: rest -> (
      let continue r = if r = [] then noop else documented_src r nbsp nbsp' in
      match line with
      | Code c -> source_code c nbsp' ++ continue rest
      | Alternative alt -> (
          match alt with Expansion e -> documented_src e.expansion nbsp nbsp')
      | Subpage p -> subpage p.content nbsp ++ continue rest
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
            let doc = match doc with [] -> noop | doc -> block doc nbsp in
            let content =
              match content with
              | `D code -> inline code nbsp
              | `N l -> documented_src l nbsp nbsp'
            in
            let item = item_prop nbsp ++ content ++ break ++ break ++ doc in
            if !(args.generate_links) then
              let anchor =
                match anchor with Some a -> a.anchor | None -> ""
              in
              break ++ break ++ anchor' anchor ++ break ++ item
            else break ++ item
          in
          let l = list ~sep:noop (List.map f lines) in
          l ++ continue rest)

and subpage { title = _; header = _; items; url = _ } nbsp =
  let content = items in
  let surround body =
    if content = [] then break else break ++ break ++ body ++ break
  in
  surround @@ item nbsp content

and item nbsp' (l : Item.t list) : Markup.t =
  match l with
  | [] -> noop
  | i :: rest -> (
      let continue r = if r = [] then noop else item nbsp' r in
      match i with
      | Text b -> block b nbsp' ++ continue rest
      | Heading h -> break ++ heading h nbsp' ++ break ++ break ++ continue rest
      | Declaration { attr = _; anchor; content; doc } ->
          let nbsp'' = nbsp ++ nbsp ++ nbsp ++ nbsp in
          let decl = documented_src content (nbsp' ++ nbsp'') nbsp' in
          let doc = match doc with [] -> noop | doc -> block doc nbsp' in
          let item' = item_prop nbsp' ++ decl ++ break ++ break ++ doc in
          if !(args.generate_links) then
            let anchor = match anchor with Some x -> x.anchor | None -> "" in
            anchor' anchor ++ break ++ item' ++ continue rest
          else item' ++ continue rest
      | Include
          { attr = _; anchor = _; content = { summary; status; content }; doc }
        ->
          let d =
            if inline_subpage status then item nbsp' content
            else
              let s = source_code summary nbsp' in
              match doc with [] -> s | doc -> s ++ block doc nbsp'
          in
          d ++ continue rest)

let on_sub subp =
  match subp with
  | `Page p -> if Link.should_inline p.Subpage.content.url then Some 1 else None
  | `Include incl -> if inline_subpage incl.Include.status then Some 0 else None

let page { Page.header; items; url; _ } =
  let header = Shift.compute ~on_sub header in
  let items = Shift.compute ~on_sub items in
  block'
    ([ inline' (Link.for_printing url) ]
    @ [ item (str "") header ++ item (str "") items ])

let rec subpage subp =
  let p = subp.Subpage.content in
  if Link.should_inline p.url then [] else [ render p ]

and render (p : Page.t) =
  let content fmt = Format.fprintf fmt "%a" Markup.pp (page p) in
  let children = Utils.flatmap ~f:subpage @@ Subpages.compute p in
  let filename = Link.as_filename p.url in
  { Odoc_document.Renderer.filename; content; children }
