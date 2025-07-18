open Odoc_document.Types
open Types
module Doctree = Odoc_document.Doctree
module Url = Odoc_document.Url

type config = {
  with_children : bool;
  shorten_beyond_depth : int option;
  remove_functor_arg_link : bool;
}

module Link = struct
  let rec flatten_path ppf (x : Odoc_document.Url.Path.t) =
    let pp_parent ppf = function
      | Some p -> Format.fprintf ppf "%a-" flatten_path p
      | None -> ()
    in
    Format.fprintf ppf "%a%a%s" pp_parent x.parent
      Url.Path.pp_disambiguating_prefix x.kind x.name

  let page p = Format.asprintf "%a" flatten_path p

  let anchor p a = Format.asprintf "%a--%s" flatten_path p a

  let label (x : Odoc_document.Url.t) =
    match x.anchor with "" -> page x.page | a -> anchor x.page a

  let rec is_inside_param (x : Odoc_document.Url.Path.t) =
    match (x.kind, x.parent) with
    | `Parameter _, _ -> true
    | _, None -> false
    | _, Some p -> is_inside_param p

  let ref config (x : Odoc_document.Url.t) =
    if config.remove_functor_arg_link && is_inside_param x.page then ""
    else label x

  let get_dir_and_file url =
    let open Odoc_document in
    let l = Url.Path.to_list url in
    let is_dir = function `Page -> `IfNotLast | _ -> `Never in
    let dir, file = Url.Path.split ~is_dir l in
    let segment_to_string (_kind, name) = name in
    ( List.map segment_to_string dir,
      String.concat "." (List.map segment_to_string file) )

  let filename ?(add_ext = true) url =
    let dir, file = get_dir_and_file url in
    let file = Fpath.(v (String.concat dir_sep (dir @ [ file ]))) in
    if add_ext then Fpath.add_ext "tex" file else file
end

module Expansion = struct
  let is_class_or_module (url : Odoc_document.Url.Path.t) =
    match url.kind with
    | `Module | `LeafPage | `Class | `Page -> true
    | _ -> false

  let shortened config status url =
    let depth x = List.length Odoc_document.Url.(Path.to_list x) in
    match (config.shorten_beyond_depth, status) with
    | None, _ | _, (`Inline | `Open | `Closed) -> false
    | Some d, `Default -> depth url >= d

  let should_inline status url =
    match status with
    | `Inline | `Open -> true
    | `Closed -> false
    | `Default ->
        (* we don't inline contents that should appear in their own page.*)
        not (is_class_or_module url)

  let remove_subpage config status url =
    shortened config status url || should_inline status url
end

let style = function
  | `Emphasis | `Italic -> Raw.emph
  | `Bold -> Raw.bold
  | `Subscript -> Raw.subscript
  | `Superscript -> Raw.superscript

let gen_hyperref pp r ppf =
  match (r.target, r.text) with
  | "", None -> ()
  | "", Some content -> Raw.inline_code pp ppf content
  | s, None -> Raw.ref ppf s
  | s, Some content ->
      let pp =
        if r.short then Raw.inline_code pp
        else fun ppf x ->
          Fmt.pf ppf "%a[p%a]" (Raw.inline_code pp) x Raw.pageref_star s
      in
      Raw.hyperref s pp ppf content

let label = function None -> [] | Some x -> [ Label (Link.label x) ]

let level_macro = function
  | 0 -> Raw.section
  | 1 -> Raw.subsection
  | 2 -> Raw.subsubsection
  | 3 | _ -> Raw.subsubsection

let none _ppf () = ()

let list kind pp ppf x =
  let list =
    match kind with Block.Ordered -> Raw.enumerate | Unordered -> Raw.itemize
  in
  let elt ppf = Raw.item pp ppf in
  match x with
  | [] -> (* empty list are not supported *) ()
  | _ -> list (Fmt.list ~sep:(fun ppf () -> Raw.break ppf Aesthetic) elt) ppf x

let escape_entity = function "#45" -> "-" | "gt" -> ">" | s -> s

let elt_size (x : elt) =
  match x with
  | Txt _ | Internal_ref _ | External_ref _ | Label _ | Style _ | Inlined_code _
  | Code_fragment _ | Tag _ | Break _ | Ligaturable _ ->
      Small
  | List _ | Section _ | Verbatim _ | Raw _ | Code_block _ | Indented _
  | Description _ | Image _ ->
      Large
  | Table _ | Layout_table _ -> Huge

let layout_table = function
  | [] -> []
  | a :: _ as m ->
      let start = List.map (fun _ -> Empty) a in
      let content_size l =
        List.fold_left (fun s x -> max s (elt_size x)) Empty l
      in
      let row mask l = List.map2 (fun x y -> max x @@ content_size y) mask l in
      let mask = List.fold_left row start m in
      let filter_empty = function
        | Empty, _ -> None
        | (Small | Large | Huge), x -> Some x
      in
      let filter_row row =
        Odoc_utils.List.filter_map filter_empty @@ List.combine mask row
      in
      let row_size = List.fold_left max Empty mask in
      [ Layout_table { row_size; tbl = List.map filter_row m } ]

let txt ~verbatim ~in_source ws =
  if verbatim then [ Txt ws ]
  else
    let escaped = List.map (Raw.Escape.text ~code_hyphenation:in_source) ws in
    match List.filter (( <> ) "") escaped with [] -> [] | l -> [ Txt l ]

let entity ~in_source ~verbatim x =
  if in_source && not verbatim then Ligaturable (escape_entity x)
  else Txt [ escape_entity x ]

(** Tables with too many rows are hard to typeset correctly on the same page.
    Splitting tables on multiple pages is unreliable with longtable + hyperref.
    Thus we limit the height of the tables that we render as latex tables. This
    variable is kept separated because we may want to make it tunable by the
    user. *)
let small_table_height_limit = 10

let rec pp_elt ppf = function
  | Txt words -> Fmt.list Fmt.string ~sep:none ppf words
  | Section { level; label; content } ->
      let with_label ppf (label, content) =
        pp ppf content;
        match label with None -> () | Some label -> Raw.label ppf label
      in
      level_macro level with_label ppf (label, content)
  | Break lvl -> Raw.break ppf lvl
  | Raw s -> Fmt.string ppf s
  | Verbatim s -> Raw.verbatim ppf s
  | Internal_ref r -> hyperref ppf r
  | External_ref (l, x) -> href ppf (l, x)
  | Style (s, x) -> style s pp ppf x
  | Code_block [] -> ()
  | Code_block x -> Raw.code_block pp ppf x
  | Inlined_code x -> Raw.inline_code pp ppf x
  | Code_fragment x -> Raw.code_fragment pp ppf x
  | List { typ; items } -> list typ pp ppf items
  | Description items -> Raw.description pp ppf items
  | Table { align; data } -> Raw.small_table pp ppf (Some align, data)
  | Layout_table { row_size = Large | Huge; tbl } -> large_table ppf tbl
  | Layout_table { row_size = Small | Empty; tbl } ->
      if List.length tbl <= small_table_height_limit then
        Raw.small_table pp ppf (None, tbl)
      else large_table ppf tbl
  | Label x -> Raw.label ppf x
  | Indented x -> Raw.indent pp ppf x
  | Ligaturable s -> Fmt.string ppf s
  | Tag (s, t) -> tag s ppf t
  | Image target -> Raw.includegraphics Fpath.pp ppf target

and pp ppf = function
  | [] -> ()
  | Break _ :: ((Layout_table _ | Table _) :: _ as q) -> pp ppf q
  | ((Layout_table _ | Table _) as t) :: Break _ :: q -> pp ppf (t :: q)
  | Break a :: Break b :: q -> pp ppf (Break (max a b) :: q)
  | Ligaturable "-" :: Ligaturable ">" :: q ->
      Raw.rightarrow ppf;
      pp ppf q
  | a :: q ->
      pp_elt ppf a;
      pp ppf q

and hyperref ppf r = gen_hyperref pp r ppf

and href ppf (l, txt) =
  match txt with
  | Some txt ->
      Raw.href l pp ppf txt;
      Raw.footnote ppf l
  | None -> Raw.url ppf l

and large_table ppf tbl =
  let rec row ppf = function
    | [] -> Raw.break ppf Line
    | [ a ] ->
        pp ppf a;
        Raw.break ppf Line
    | [ a; b ] -> Fmt.pf ppf "%a%a%a" pp a Raw.break Aesthetic (Raw.indent pp) b
    | a :: (_ :: _ as q) ->
        Fmt.pf ppf "%a%a%a" pp a Raw.break Aesthetic (Raw.indent row) q
  in
  let matrix ppf m = List.iter (row ppf) m in
  Raw.indent matrix ppf tbl

and tag s ppf x = Raw.ocamltag s pp ppf x

let raw_markup (t : Raw_markup.t) =
  let target, content = t in
  match Astring.String.Ascii.lowercase target with
  | "latex" | "tex" -> [ Raw content ]
  | _ -> []

let source k (t : Source.t) =
  let rec token (x : Source.token) =
    match x with
    | Elt i -> k i
    | Tag (None, l) -> tokens l
    | Tag (Some s, l) -> [ Tag (s, tokens l) ]
  and tokens t = Odoc_utils.List.concat_map token t in
  tokens t

let rec internalref ~config ~verbatim ~in_source (t : Target.internal)
    (c : Inline.t) =
  let target =
    match t with
    | Target.Resolved uri -> Link.ref config uri
    | Unresolved -> "xref-unresolved"
  in
  let text = inline ~config ~verbatim ~in_source c in
  let short = in_source in
  Internal_ref { short; target; text = Some text }

and inline ~config ~in_source ~verbatim (l : Inline.t) =
  let one (t : Inline.one) =
    match t.desc with
    | Text _s -> assert false
    | Linebreak -> [ Break Line ]
    | Styled (style, c) ->
        [ Style (style, inline ~config ~verbatim ~in_source c) ]
    | Link { target = External ext; content = c; _ } ->
        let content = inline ~config ~verbatim:false ~in_source:false c in
        [ External_ref (ext, Some content) ]
    | Link { target = Internal ref_; content = c; _ } ->
        [ internalref ~config ~in_source ~verbatim ref_ c ]
    | Source c ->
        [
          Inlined_code
            (source (inline ~config ~verbatim:false ~in_source:true) c);
        ]
    | Math s -> [ Raw (Format.asprintf "%a" Raw.math s) ]
    | Raw_markup r -> raw_markup r
    | Entity s -> [ entity ~in_source ~verbatim s ]
  in

  let take_text (l : Inline.t) =
    Doctree.Take.until l ~classify:(function
      | { Inline.desc = Text code; _ } -> Accum [ code ]
      | { desc = Entity e; _ } -> Accum [ escape_entity e ]
      | _ -> Stop_and_keep)
  in
  (* if in_source then block_code_txt s else if_not_empty (fun x -> Txt x) s *)
  let rec prettify = function
    | { Inline.desc = Inline.Text _; _ } :: _ as l ->
        let words, _, rest = take_text l in
        txt ~in_source ~verbatim words @ prettify rest
    | o :: q -> one o @ prettify q
    | [] -> []
  in
  prettify l

let heading ~config p (h : Heading.t) =
  let content = inline ~config ~in_source:false ~verbatim:false h.title in
  [
    Section
      { label = Option.map (Link.anchor p) h.label; level = h.level; content };
    Break Aesthetic;
  ]

let non_empty_block_code ~config c =
  let s = source (inline ~config ~verbatim:true ~in_source:true) c in
  match s with
  | [] -> []
  | _ :: _ as l -> [ Break Separation; Code_block l; Break Separation ]

let non_empty_code_fragment ~config c =
  let s = source (inline ~config ~verbatim:false ~in_source:true) c in
  match s with [] -> [] | _ :: _ as l -> [ Code_fragment l ]

let alt_text ~in_source (target : Target.t) alt =
  let text = txt ~verbatim:false ~in_source:false [ alt ] in
  let break = if in_source then [] else [ Break Paragraph ] in
  match target with
  | Internal _ -> text @ break
  | External l -> [ External_ref (l, Some text) ] @ break

let image ~in_source (internal_url : Url.t) alt =
  let dir, file = Link.get_dir_and_file internal_url.page in
  match Fpath.(get_ext @@ v file) with
  (* list imported from pdftex.def *)
  | "" | ".pdf" | ".png" | ".jpg" | ".mps" | ".jpeg" | ".jbig2" | ".jb2"
  | ".PDF" | ".PNG" | ".JPG" | ".JPEG" | ".JBIG2" | ".JB2" ->
      let fpath = Fpath.v (String.concat Fpath.dir_sep (dir @ [ file ])) in
      [ Image fpath ]
  | _ -> alt_text ~in_source (Internal (Resolved internal_url)) alt

let rec block ~config ~in_source (l : Block.t) =
  let one (t : Block.one) =
    match t.desc with
    | Inline i -> inline ~config ~verbatim:false ~in_source:false i
    | Image (Internal (Resolved x), alt) -> image ~in_source x alt
    | Image (t, alt) | Audio (t, alt) | Video (t, alt) ->
        alt_text ~in_source t alt
    | Paragraph i ->
        inline ~config ~in_source:false ~verbatim:false i
        @ if in_source then [] else [ Break Paragraph ]
    | List (typ, l) ->
        [ List { typ; items = List.map (block ~config ~in_source:false) l } ]
    | Table t -> table_block ~config t
    | Description l ->
        [
          (let item i =
             ( inline ~config ~in_source ~verbatim:false i.Description.key,
               block ~config ~in_source i.Description.definition )
           in
           Description (List.map item l));
        ]
    | Raw_markup r -> raw_markup r
    | Verbatim s -> [ Verbatim s ]
    | Source (_, c) -> non_empty_block_code ~config c
    | Math s ->
        [
          Break Paragraph;
          Raw (Format.asprintf "%a" Raw.equation s);
          Break Paragraph;
        ]
  in
  Odoc_utils.List.concat_map one l

and table_block ~config { Table.data; align } =
  let data =
    List.map
      (List.map (fun (cell, cell_type) ->
           let content = block ~config ~in_source:false cell in
           match cell_type with
           | `Header -> [ Style (`Bold, content) ]
           | `Data -> content))
      data
  in
  [ Table { align; data } ]

let rec is_only_text l =
  let is_text : Item.t -> _ = function
    | Heading _ | Text _ -> true
    | Declaration _ -> false
    | Include { content = items; _ } -> is_only_text items.content
  in
  List.for_all is_text l

let rec documentedSrc ~config (t : DocumentedSrc.t) =
  let open DocumentedSrc in
  let rec to_latex t =
    match t with
    | [] -> []
    | Code _ :: _ ->
        let take_code l =
          Doctree.Take.until l ~classify:(function
            | Code code -> Accum code
            | _ -> Stop_and_keep)
        in
        let code, _, rest = take_code t in
        non_empty_code_fragment ~config code @ to_latex rest
    | Alternative (Expansion e) :: rest ->
        let elt =
          (* In the [should_inline] or [shortened], we are replacing the
           independent page by the inlined contents, thus we need to redirect
           the links to the missing page to the inlined contents.
           redirect the *)
          if Expansion.should_inline e.status e.url then
            Label (Link.page e.url) :: to_latex e.expansion
          else if Expansion.shortened config e.status e.url then
            Label (Link.page e.url) :: non_empty_code_fragment ~config e.summary
          else non_empty_code_fragment ~config e.summary
        in
        elt @ to_latex rest
    | Subpage subp :: rest ->
        Indented (items ~config subp.content.url subp.content.items)
        :: to_latex rest
    | (Documented _ | Nested _) :: _ ->
        let take_descr l =
          Doctree.Take.until l ~classify:(function
            | Documented { attrs; anchor; code; doc; markers } ->
                Accum
                  [
                    {
                      DocumentedSrc.attrs;
                      anchor;
                      code = `D code;
                      doc;
                      markers;
                    };
                  ]
            | Nested { attrs; anchor; code; doc; markers } ->
                Accum
                  [
                    {
                      DocumentedSrc.attrs;
                      anchor;
                      code = `N code;
                      doc;
                      markers;
                    };
                  ]
            | _ -> Stop_and_keep)
        in
        let l, _, rest = take_descr t in
        let one dsrc =
          let content =
            match dsrc.code with
            | `D code -> inline ~config ~verbatim:false ~in_source:true code
            | `N n -> to_latex n
          in
          let doc = [ block ~config ~in_source:true dsrc.doc ] in
          (content @ label dsrc.anchor) :: doc
        in
        layout_table (List.map one l) @ to_latex rest
  in
  to_latex t

and items ~config page_url l =
  let rec walk_items ~page_url ~only_text acc (t : Item.t list) =
    let continue_with rest elts =
      walk_items ~page_url ~only_text (List.rev_append elts acc) rest
    in
    match t with
    | [] -> List.rev acc
    | Text _ :: _ as t ->
        let text, _, rest =
          Doctree.Take.until t ~classify:(function
            | Item.Text text -> Accum text
            | _ -> Stop_and_keep)
        in
        let content = block ~config ~in_source:false text in
        let elts = content in
        elts |> continue_with rest
    | Heading h :: rest -> heading ~config page_url h |> continue_with rest
    | Include
        {
          attr = _;
          source_anchor = _;
          anchor;
          doc;
          content = { summary; status = _; content };
        }
      :: rest ->
        let included = items page_url content in
        let docs = block ~config ~in_source:true doc in
        let summary =
          source (inline ~config ~verbatim:false ~in_source:true) summary
        in
        let content = included in
        label anchor @ docs @ summary @ content |> continue_with rest
    | Declaration { Item.attr = _; source_anchor = _; anchor; content; doc }
      :: rest ->
        let content = label anchor @ documentedSrc ~config content in
        let elts =
          match doc with
          | [] -> content @ [ Break Line ]
          | docs ->
              content
              @ [
                  Indented (block ~config ~in_source:true docs);
                  Break Separation;
                ]
        in
        continue_with rest elts
  and items page_url l =
    walk_items ~page_url ~only_text:(is_only_text l) [] l
  in
  items page_url l

module Doc = struct
  let link_children ppf children =
    let input_child ppf child =
      Raw.input ppf child.Odoc_document.Renderer.filename
    in
    Fmt.list input_child ppf children

  let make ~config url content children =
    let filename = Link.filename url in
    let label = Label (Link.page url) in
    let content =
      match content with
      | [] -> [ label ]
      | (Section _ as s) :: q -> s :: label :: q
      | q -> label :: q
    in
    let children_input ppf =
      if config.with_children then link_children ppf children else ()
    in
    let content ppf = Fmt.pf ppf "@[<v>%a@,%t@]@." pp content children_input in
    { Odoc_document.Renderer.filename; content; children; path = url }
end

module Page = struct
  let on_sub = function `Page _ -> Some 1 | `Include _ -> None

  let rec subpage ~config (p : Subpage.t) =
    if Expansion.remove_subpage config p.status p.content.url then []
    else [ page ~config p.content ]

  and subpages ~config subpages =
    List.flatten @@ List.map (subpage ~config) subpages

  and page ~config p =
    let { Page.items = i; url; _ } =
      Doctree.Labels.disambiguate_page ~enter_subpages:true p
    and subpages = subpages ~config @@ Doctree.Subpages.compute p in
    let i = Doctree.Shift.compute ~on_sub i in
    let header, preamble = Doctree.PageTitle.render_title p in
    let header = items ~config url (header @ preamble) in
    let content = items ~config url i in
    let page = Doc.make ~config url (header @ content) subpages in
    page
end

let render ~config = function
  | Document.Page page -> [ Page.page ~config page ]
  | Source_page _ -> []

let filepath url = Link.filename ~add_ext:false url
