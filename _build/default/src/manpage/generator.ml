module ManLink = Link
open Odoc_document
open Types
open Doctree
module Link = ManLink

(*
Manpages relies on the (g|t|n)roff document language.
This language has a fairly long history
(see https://en.wikipedia.org/wiki/Groff_(software)).

Unfortunately, this language is very old and quite clunky.
Most manpages relies on a set of high-level macros
(http://man7.org/linux/man-pages/man7/groff_man.7.html)
that attempts to represent the semantic of common constructs in man pages. These
macros are too constraining for the rich ocamldoc markup and
their semantics are quite brittle, making them hard to use in a machine-output
context.

For these reason, we hit the low level commands directly:
- http://man7.org/linux/man-pages/man7/groff.7.html
- http://mandoc.bsd.lv/man/roff.7.html

The downside of these commands is their poor translation to HTML, which we
don't care about.

In the roff language:
1) newlines are not distinguished from other whitespace
2) Successive whitespaces are ignored, except to trigger
   "end of sentence detection" for 2 or more successive whitespaces.
3) Commands must start at the beginning of a line.
4) Whitespaces separated by a macro are not treated as a single whitespace.

For all these reasons, We use a concatenative API that will gobble up adjacent
extra whitespaces and never output successive whitespaces at all.
This makes the output much more consistent.
*)
module Roff = struct
  type t =
    | Concat of t list
    | Font of string * t
    | Macro of string * string
    | Space
    | Break
    | String of string
    | Vspace
    | Indent of int * t
    | Align_line of string
    | Table_cell of t

  let noop = Concat []

  let sp = Space

  let break = Break

  let vspace = Vspace

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

  let indent i content = Indent (i, content)

  let macro id fmt = Format.ksprintf (fun s -> Macro (id, s)) fmt

  (* copied from cmdliner *)
  let escape s =
    (* escapes [s] from doc language. *)
    let markup_text_need_esc = function '.' | '\\' -> true | _ -> false in
    let max_i = String.length s - 1 in
    let rec escaped_len i l =
      if i > max_i then l
      else if markup_text_need_esc s.[i] then escaped_len (i + 1) (l + 2)
      else escaped_len (i + 1) (l + 1)
    in
    let escaped_len = escaped_len 0 0 in
    if escaped_len = String.length s then s
    else
      let b = Bytes.create escaped_len in
      let rec loop i k =
        if i > max_i then Bytes.unsafe_to_string b
        else
          let c = String.unsafe_get s i in
          if not (markup_text_need_esc c) then (
            Bytes.unsafe_set b k c;
            loop (i + 1) (k + 1))
          else (
            Bytes.unsafe_set b k '\\';
            Bytes.unsafe_set b (k + 1) c;
            loop (i + 1) (k + 2))
      in
      loop 0 0

  let str fmt = Format.ksprintf (fun s -> String (escape s)) fmt

  let escaped fmt = Format.ksprintf (fun s -> String s) fmt

  let env o c arg content = macro o "%s" arg ++ content ++ macro c ""

  let font s content = Font (s, content)

  let font_stack = Stack.create ()

  let pp_font ppf s fmt =
    let command_f ppf s =
      if String.length s = 1 then Format.fprintf ppf {|\f%s|} s
      else Format.fprintf ppf {|\f[%s]|} s
    in
    Stack.push s font_stack;
    command_f ppf s;
    Format.kfprintf
      (fun ppf ->
        ignore @@ Stack.pop font_stack;
        let s =
          if Stack.is_empty font_stack then "R" else Stack.top font_stack
        in
        command_f ppf s)
      ppf fmt

  let collapse x =
    let skip_spaces l =
      let _, _, rest =
        Take.until l ~classify:(function Space -> Skip | _ -> Stop_and_keep)
      in
      rest
    and skip_spaces_and_break l =
      let _, _, rest =
        Take.until l ~classify:(function
          | Space | Break -> Skip
          | _ -> Stop_and_keep)
      in
      rest
    and skip_spaces_and_break_and_vspace l =
      let _, _, rest =
        Take.until l ~classify:(function
          | Space | Break | Vspace -> Skip
          | _ -> Stop_and_keep)
      in
      rest
    in
    let rec loop acc l =
      match l with
      (* | (Space | Break) :: (Macro _ :: _ as t) ->
       *   loop acc t *)
      | Vspace :: _ ->
          let rest = skip_spaces_and_break_and_vspace l in
          loop (Vspace :: acc) rest
      | Break :: _ ->
          let rest = skip_spaces_and_break l in
          loop (Break :: acc) rest
      | Space :: _ ->
          let rest = skip_spaces l in
          loop (Space :: acc) rest
      | Concat l :: rest -> loop acc (l @ rest)
      | (Macro _ as h) :: rest ->
          let rest = skip_spaces rest in
          loop (h :: acc) rest
      | [] -> acc
      | h :: t -> loop (h :: acc) t
    in
    List.rev @@ loop [] [ x ]

  let rec next_is_macro = function
    | (Vspace | Break | Macro _) :: _ -> true
    | Concat l :: _ -> next_is_macro l
    | Font (_, content) :: _ | Indent (_, content) :: _ ->
        next_is_macro [ content ]
    | _ -> false

  let pp_macro ppf s fmt = Format.fprintf ppf ("@\n.%s " ^^ fmt) s

  let pp_indent ppf indent =
    if indent = 0 then () else pp_macro ppf "ti" "+%d" indent

  let newline_if ppf b = if b then Format.pp_force_newline ppf () else ()

  let pp ppf t =
    let rec many ~indent ppf l =
      match l with
      | [] -> ()
      | h :: t ->
          let is_macro = next_is_macro t in
          (match h with
          | Concat l -> many ~indent ppf l
          | String s -> Format.pp_print_string ppf s
          | Font (s, t) -> pp_font ppf s "%a" (one ~indent) t
          | Space -> Format.fprintf ppf " "
          | Break ->
              pp_macro ppf "br" "";
              pp_indent ppf indent;
              newline_if ppf (not is_macro)
          | Vspace ->
              pp_macro ppf "sp" "";
              pp_indent ppf indent;
              newline_if ppf (not is_macro)
          | Macro (s, args) ->
              pp_macro ppf s "%s" args;
              newline_if ppf (not is_macro)
          | Align_line s ->
              Format.pp_print_string ppf (s ^ ".");
              newline_if ppf (not is_macro)
          | Table_cell c ->
              Format.pp_print_text ppf "T{\n";
              one ~indent ppf c;
              Format.pp_print_text ppf "\nT}"
          | Indent (i, content) ->
              let indent = indent + i in
              one ~indent ppf content);
          many ~indent ppf t
    and one ~indent ppf x = many ~indent ppf @@ collapse x in
    Format.pp_set_margin ppf max_int;
    one ~indent:0 ppf t
end

open Roff

let style (style : style) content =
  match style with
  | `Bold -> font "B" content
  | `Italic -> font "I" content
  (* We ignore those *)
  | `Emphasis | `Superscript | `Subscript -> content

(* Striped content should be rendered in one line, without styling *)
let strip l =
  let rec loop acc = function
    | [] -> acc
    | h :: t -> (
        match h.Inline.desc with
        | Text _ | Entity _ | Raw_markup _ | Math _ -> loop (h :: acc) t
        | Linebreak -> loop acc t
        | Styled (sty, content) ->
            let h =
              { h with desc = Styled (sty, List.rev @@ loop [] content) }
            in
            loop (h :: acc) t
        | Link { content; _ } ->
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
  match e with "#45" -> escaped "\\-" | "gt" -> str ">" | s -> str "&%s;" s

(* Should hopefully make people notice and report *)

let raw_markup (t : Raw_markup.t) =
  let target, content = t in
  match Astring.String.Ascii.lowercase target with
  | "manpage" | "troff" | "roff" -> String content
  | _ -> noop

let math (s : Types.Math.t) = String s

let rec source_code (s : Source.t) =
  match s with
  | [] -> noop
  | h :: t -> (
      match h with
      | Source.Elt i -> inline (strip i) ++ source_code t
      | Tag (None, s) -> source_code s ++ source_code t
      | Tag (Some _, s) -> font "CB" (source_code s) ++ source_code t)

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
      | Link { target = External href; content; _ } ->
          env "UR" "UE" href (inline @@ strip content) ++ inline rest
      | Link { content; _ } ->
          font "CI" (inline @@ strip content) ++ inline rest
      | Source content -> source_code content ++ inline rest
      | Math s -> math s ++ inline rest
      | Raw_markup t -> raw_markup t ++ inline rest)

let table pp { Table.data; align } =
  let sep = '\t' in
  let alignment =
    let alignment =
      match align with
      | align ->
          List.map
            (function
              (* Since we are enclosing cells in text blocks, the alignment has
                 no effect on the content of a sufficiently big cell, for some
                 reason... (see the markup test in generators)

                 One solution would be to use the [m] column specifier to apply
                 a macro to the text blocks of the columns. Those macros would
                 be [lj], [ce] or [rj], which define alignment. However, this
                 breaks both the alignment for small table cells, and the
                 largeness of columns. For the records, it woulb be:

                 {[
                   | Some `Left -> "lmlj"
                   | Some `Center -> "cmce"
                   | Some `Right -> "rmrj"
                   | None -> "l"
                  ]} *)
              | Table.Left -> "l"
              | Center -> "c"
              | Right -> "r"
              | Default -> "l")
            align
    in
    Align_line (String.concat "" alignment)
  in
  env "TS" "TE" ""
    (str "allbox;" ++ alignment
    ++ List.fold_left
         (fun acc row ->
           acc ++ vspace
           ++
           match row with
           | [] -> noop
           | (h, _) :: t ->
               List.fold_left
                 (fun acc (x, _) -> acc ++ str "%c" sep ++ Table_cell (pp x))
                 (Table_cell (pp h))
                 t)
         noop data)

let rec block (l : Block.t) =
  match l with
  | [] -> noop
  | b :: rest -> (
      let continue r = if r = [] then noop else vspace ++ block r in
      match b.desc with
      | Inline i -> inline i ++ continue rest
      | Video (_, content) | Audio (_, content) | Image (_, content) ->
          str "%s" content ++ continue rest
      | Paragraph i -> inline i ++ continue rest
      | List (list_typ, l) ->
          let f n b =
            let bullet =
              match list_typ with
              | Unordered -> escaped {|\(bu|}
              | Ordered -> str "%d)" (n + 1)
            in
            indent 2 (bullet ++ sp ++ block b)
          in
          list ~sep:break (List.mapi f l) ++ continue rest
      | Table t -> table block t ++ continue rest
      | Description _ ->
          let descrs, _, rest =
            Take.until l ~classify:(function
              | { Block.desc = Description l; _ } -> Accum l
              | _ -> Stop_and_keep)
          in
          let f i =
            let key = inline i.Description.key in
            let def = block i.Description.definition in
            indent 2 (str "@" ++ key ++ str ":" ++ sp ++ def)
          in
          list ~sep:break (List.map f descrs) ++ continue rest
      | Source (_, content) ->
          env "EX" "EE" "" (source_code content) ++ continue rest
      | Math s -> math s ++ continue rest
      | Verbatim content -> env "EX" "EE" "" (str "%s" content) ++ continue rest
      | Raw_markup t -> raw_markup t ++ continue rest)

let next_heading, reset_heading =
  let heading_stack = ref [] in
  let rec succ_heading i l =
    match (i, l) with
    | 1, [] -> [ 1 ]
    | _, [] -> 1 :: succ_heading (i - 1) []
    | 1, n :: _ -> [ n + 1 ]
    | i, n :: t -> n :: succ_heading (i - 1) t
  in
  let print_heading l = String.concat "." @@ List.map string_of_int l in
  let next level =
    let new_heading = succ_heading level !heading_stack in
    heading_stack := new_heading;
    print_heading new_heading
  and reset () = heading_stack := [] in
  (next, reset)

let heading ~nested { Heading.label = _; level; title; source_anchor = _ } =
  let prefix =
    if level = 0 then noop
    else if level <= 3 then str "%s " (next_heading level)
    else noop
  in
  if not nested then
    macro "in" "%d" (level + 2)
    ++ font "B" (prefix ++ inline (strip title))
    ++ macro "in" ""
  else font "B" (prefix ++ inline (strip title))

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
      let break_if_nonempty r = if r = [] then noop else break in
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
      | Subpage p -> subpage p.content ++ continue rest
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
              | doc ->
                  indent 2
                    (break ++ str "(*" ++ sp ++ block doc ++ sp ++ str "*)")
            in
            let content =
              match content with
              | `D code -> inline code
              | `N l -> indent 2 (documentedSrc l)
            in
            content ++ doc
          in
          let l = list ~sep:break (List.map f lines) in
          indent 2 (break ++ l) ++ break_if_nonempty rest ++ continue rest)

and subpage { preamble = _; items; url = _; _ } =
  let content = items in
  let surround body =
    if content = [] then sp else indent 2 (break ++ body) ++ break
  in
  surround @@ item ~nested:true content

and item ~nested (l : Item.t list) =
  match l with
  | [] -> noop
  | i :: rest -> (
      let continue r = if r = [] then noop else vspace ++ item ~nested r in
      match i with
      | Text b ->
          let d = env "fi" "nf" "" (block b) in
          d ++ continue rest
      | Heading h ->
          let h = heading ~nested h in
          vspace ++ h ++ vspace ++ item ~nested rest
      | Declaration { attr = _; anchor = _; source_anchor = _; content; doc } ->
          let decl = documentedSrc content in
          let doc =
            match doc with
            | [] -> noop
            | doc -> env "fi" "nf" "" (indent 2 (break ++ block doc))
          in
          decl ++ doc ++ continue rest
      | Include
          {
            attr = _;
            anchor = _;
            source_anchor = _;
            content = { summary; status; content };
            doc;
          } ->
          let d =
            if inline_subpage status then item ~nested content
            else
              let s = source_code summary in
              match doc with
              | [] -> s
              | doc -> s ++ indent 2 (break ++ block doc)
          in
          d ++ continue rest)

let on_sub subp =
  match subp with
  | `Page p -> if Link.should_inline p.Subpage.content.url then Some 1 else None
  | `Include incl -> if inline_subpage incl.Include.status then Some 0 else None

let page p =
  reset_heading ();
  let header =
    Doctree.PageTitle.render_title p @ Shift.compute ~on_sub p.preamble
  in
  let i = Shift.compute ~on_sub p.items in
  macro "TH" {|%s 3 "" "Odoc" "OCaml Library"|} p.url.name
  ++ macro "SH" "Name"
  ++ str "%s" (String.concat "." @@ Link.for_printing p.url)
  ++ macro "SH" "Synopsis" ++ vspace ++ item ~nested:false header
  ++ macro "SH" "Documentation" ++ vspace ++ macro "nf" ""
  ++ item ~nested:false i

let rec subpage subp =
  let p = subp.Subpage.content in
  if Link.should_inline p.url then [] else [ render_page p ]

and render_page (p : Page.t) =
  let p = Doctree.Labels.disambiguate_page ~enter_subpages:true p
  and children = Utils.flatmap ~f:subpage @@ Subpages.compute p in
  let content ppf = Format.fprintf ppf "%a@." Roff.pp (page p) in
  let filename = Link.as_filename p.url in
  { Renderer.filename; content; children; path = p.url }

let render = function
  | Document.Page page -> [ render_page page ]
  | Source_page _ -> []

let filepath url = Link.as_filename ~add_ext:false url
