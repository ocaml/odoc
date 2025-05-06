open Type_desc
open Odoc_model
open Comment
open Paths_desc

let ignore_loc x = x.Location_.value

let media =
  Variant
    (function
    | `Link -> C0 "`Link"
    | `Audio -> C0 "`Audio"
    | `Video -> C0 "`Video"
    | `Image -> C0 "`Image")

let rec leaf_inline_element_fn : Odoc_model.Comment.leaf_inline_element -> case
    = function
  | `Space -> C0 "`Space"
  | `Word x -> C ("`Word", x, string)
  | `Code_span x -> C ("`Code_span", x, string)
  | `Math_span x -> C ("`Math_span", x, string)
  | `Raw_markup (x1, x2) -> C ("`Raw_markup", (x1, x2), Pair (string, string))

and style =
  Variant
    (function
    | `Bold -> C0 "`Bold"
    | `Italic -> C0 "`Italic"
    | `Emphasis -> C0 "`Emphasis"
    | `Superscript -> C0 "`Superscript"
    | `Subscript -> C0 "`Subscript")

and non_link_inline_element_fn : non_link_inline_element -> case = function
  | #leaf_inline_element as x -> leaf_inline_element_fn x
  | `Styled (x1, x2) -> C ("`Styled", (x1, x2), Pair (style, link_content))

and reference_element_fn : reference_element -> case = function
  | `Reference (x1, x2) ->
      C ("`Reference", (x1, x2), Pair (reference, link_content))

and inline_element_fn : inline_element -> case = function
  | #leaf_inline_element as x -> leaf_inline_element_fn x
  | #reference_element as x -> reference_element_fn x
  | `Styled (x1, x2) -> C ("`Styled", (x1, x2), Pair (style, paragraph))
  | `Link (x1, x2) -> C ("`Link", (x1, x2), Pair (string, link_content))

and link_content =
  List (Indirect (ignore_loc, Variant non_link_inline_element_fn))

and inline_element = Variant inline_element_fn

and paragraph = List (Indirect (ignore_loc, inline_element))

let module_reference =
  let simplify m =
    ((m.module_reference :> Paths.Reference.t), m.module_synopsis)
  in
  Indirect (simplify, Pair (reference, Option paragraph))

let heading =
  let heading_level =
    Variant
      (function
      | `Title -> C0 "`Title"
      | `Section -> C0 "`Section"
      | `Subsection -> C0 "`Subsection"
      | `Subsubsection -> C0 "`Subsubsection"
      | `Paragraph -> C0 "`Paragraph"
      | `Subparagraph -> C0 "`Subparagraph")
  in
  let heading_attrs =
    Record
      [
        F ("heading_level", (fun h -> h.heading_level), heading_level);
        F ("heading_label_explicit", (fun h -> h.heading_label_explicit), bool);
      ]
  in
  Triple (heading_attrs, identifier, paragraph)

let media_href =
  Variant
    (function
    | `Reference r ->
        C ("`Reference", (r : Reference.Asset.t :> Reference.t), reference)
    | `Link l -> C ("`Link", l, string))

let code_block_tag : Odoc_parser.Ast.code_block_tag t =
  Variant
    (function
    | `Tag x -> C ("`Tag", ignore_loc x, string)
    | `Binding (x1, x2) ->
        C ("`Binding", (ignore_loc x1, ignore_loc x2), Pair (string, string)))

let code_block_meta : Odoc_parser.Ast.code_block_meta t =
  Record
    [
      F ("language", (fun h -> ignore_loc h.language), string);
      F ("warnings_tag", (fun h -> h.tags), List code_block_tag);
    ]

let rec code_block : code_block t =
  Record
    [
      F ("meta", (fun h -> h.meta), Option code_block_meta);
      F ("delimiter", (fun h -> h.delimiter), Option string);
      F ("content", (fun h -> ignore_loc h.content), string);
      F ("output", (fun h -> h.output), Option nestable_elements);
    ]

and nestable_block_element_fn : nestable_block_element -> case =
  let list_kind =
    Variant
      (function `Unordered -> C0 "`Unordered" | `Ordered -> C0 "`Ordered")
  in
  function
  | `Paragraph x -> C ("`Paragraph", x, paragraph)
  | `Code_block c -> C ("`Code_block", c, code_block)
  | `Math_block x -> C ("`Math_block", x, string)
  | `Verbatim x -> C ("`Verbatim", x, string)
  | `Modules x -> C ("`Modules", x, List module_reference)
  | `Table { data; align } ->
      let cell_type_desc =
        Variant (function `Header -> C0 "`Header" | `Data -> C0 "`Data")
      in
      let data_desc = List (List (Pair (nestable_elements, cell_type_desc))) in
      let align_desc =
        Option
          (Variant
             (function
             | `Left -> C0 "`Left"
             | `Center -> C0 "`Center"
             | `Right -> C0 "`Right"))
      in
      let align_desc = List align_desc in
      let table_desc = Pair (data_desc, Option align_desc) in
      C ("`Table", (data, align), table_desc)
  | `List (x1, x2) ->
      C ("`List", (x1, x2), Pair (list_kind, List nestable_elements))
  | `Media (x1, m, x2) ->
      C ("`Media", (x1, m, x2), Triple (media_href, media, string))

and nestable_block_element : nestable_block_element t =
  Variant nestable_block_element_fn

and nestable_elements : nestable_block_element with_location list t =
  List
    (Indirect
       ( (fun x ->
           let x :> nestable_block_element Location_.with_location = x in
           ignore_loc x),
         nestable_block_element ))

and block_element : block_element t =
  Variant
    (function
    | #nestable_block_element as x -> nestable_block_element_fn x
    | `Heading (x1, x2, x3) ->
        C ("`Heading", (x1, (x2 :> Identifier.t), x3), heading)
    | `Tag x -> C ("`Tag", x, tag))

and tag : tag t =
  let url_kind =
    Variant
      (function
      | `Url -> C0 "`Url"
      | `File -> C0 "`File"
      | `Document -> C0 "`Document")
  in
  Variant
    (function
    | `Author x -> C ("`Author", x, string)
    | `Deprecated x -> C ("`Deprecated", x, nestable_elements)
    | `Param (x1, x2) -> C ("`Param", (x1, x2), Pair (string, nestable_elements))
    | `Raise (`Code_span x1, x2) ->
        C ("`Raise", (x1, x2), Pair (string, nestable_elements))
    | `Raise (`Reference (x1, x2), x3) ->
        C
          ( "`Raise",
            (x1, x2, x3),
            Triple (reference, link_content, nestable_elements) )
    | `Return x -> C ("`Return", x, nestable_elements)
    | `See (x1, x2, x3) ->
        C ("`See", (x1, x2, x3), Triple (url_kind, string, nestable_elements))
    | `Since x -> C ("`Since", x, string)
    | `Before (x1, x2) ->
        C ("`Before", (x1, x2), Pair (string, nestable_elements))
    | `Version x -> C ("`Version", x, string)
    | `Alert (x1, x2) -> C ("`Alert", (x1, x2), Pair (string, Option string))
    | `Custom (x1, x2) -> C ("`" ^ x1, x2, nestable_elements))

let elements : elements t = List (Indirect (ignore_loc, block_element))

let docs =
  Record
    [
      F ("elements", (fun h -> h.elements), elements);
      F ("warnings_tag", (fun h -> h.warnings_tag), Option string);
    ]

let docs_or_stop : docs_or_stop t =
  Variant (function `Docs x -> C ("`Docs", x, docs) | `Stop -> C0 "`Stop")

let inline_elements : inline_element with_location list t =
  List (Indirect (ignore_loc, inline_element))