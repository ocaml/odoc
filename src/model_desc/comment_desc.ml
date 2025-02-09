open Type_desc
open Odoc_model
open Comment
open Paths_desc

let ignore_loc x = x.Location_.value

type general_inline_element =
  [ `Space
  | `Word of string
  | `Code_span of string
  | `Math_span of string
  | `Raw_markup of raw_markup_target * string
  | `Styled of style * general_inline_element with_location list
  | `Reference of Paths.Reference.t * general_link_content
  | `Link of string * general_link_content ]

and general_link_content = general_inline_element with_location list

type general_block_element =
  [ `Paragraph of general_link_content
  | `Code_block of
    string option
    * string with_location
    * general_block_element with_location list option
  | `Math_block of string
  | `Verbatim of string
  | `Modules of Comment.module_reference list
  | `List of
    [ `Unordered | `Ordered ] * general_block_element with_location list list
  | `Table of general_block_element abstract_table
  | `Heading of
    Comment.heading_attrs * Identifier.Label.t * general_link_content
  | `Tag of general_tag
  | `Media of
    [ `Reference of Paths.Reference.t | `Link of string ] * media * string ]

and general_tag =
  [ `Author of string
  | `Deprecated of general_docs
  | `Param of string * general_docs
  | `Raise of
    [ `Code_span of string
    | `Reference of Paths.Reference.t * general_link_content ]
    * general_docs
  | `Return of general_docs
  | `See of [ `Url | `File | `Document ] * string * general_docs
  | `Since of string
  | `Before of string * general_docs
  | `Version of string
  | `Alert of string * string option ]

and general_docs = general_block_element with_location list

let media =
  Variant
    (function
    | `Link -> C0 "`Link"
    | `Audio -> C0 "`Audio"
    | `Video -> C0 "`Video"
    | `Image -> C0 "`Image")

let rec inline_element : general_inline_element t =
  let style =
    Variant
      (function
      | `Bold -> C0 "`Bold"
      | `Italic -> C0 "`Italic"
      | `Emphasis -> C0 "`Emphasis"
      | `Superscript -> C0 "`Superscript"
      | `Subscript -> C0 "`Subscript")
  in
  Variant
    (function
    | `Space -> C0 "`Space"
    | `Word x -> C ("`Word", x, string)
    | `Code_span x -> C ("`Code_span", x, string)
    | `Math_span x -> C ("`Math_span", x, string)
    | `Raw_markup (x1, x2) -> C ("`Raw_markup", (x1, x2), Pair (string, string))
    | `Styled (x1, x2) -> C ("`Styled", (x1, x2), Pair (style, link_content))
    | `Reference (x1, x2) ->
        C ("`Reference", (x1, x2), Pair (reference, link_content))
    | `Link (x1, x2) -> C ("`Link", (x1, x2), Pair (string, link_content)))

and link_content : general_link_content t =
  List (Indirect (ignore_loc, inline_element))

let module_reference =
  let simplify m =
    ( (m.module_reference :> Paths.Reference.t),
      (m.module_synopsis :> general_link_content option) )
  in
  Indirect (simplify, Pair (reference, Option link_content))

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
  Triple (heading_attrs, identifier, link_content)

let media_href =
  Variant
    (function
    | `Reference r -> C ("`Reference", r, reference)
    | `Link l -> C ("`Link", l, string))

let rec block_element : general_block_element t =
  let list_kind =
    Variant
      (function `Unordered -> C0 "`Unordered" | `Ordered -> C0 "`Ordered")
  in
  Variant
    (function
    | `Paragraph x -> C ("`Paragraph", x, link_content)
    | `Code_block (x1, x2, _) ->
        C ("`Code_block", (x1, ignore_loc x2), Pair (Option string, string))
    | `Math_block x -> C ("`Math_block", x, string)
    | `Verbatim x -> C ("`Verbatim", x, string)
    | `Modules x -> C ("`Modules", x, List module_reference)
    | `List (x1, x2) ->
        C
          ( "`List",
            (x1, (x2 :> general_docs list)),
            Pair (list_kind, List general_content) )
    | `Table { data; align } ->
        let cell_type_desc =
          Variant (function `Header -> C0 "`Header" | `Data -> C0 "`Data")
        in
        let data_desc = List (List (Pair (general_content, cell_type_desc))) in
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
    | `Heading h -> C ("`Heading", h, heading)
    | `Tag x -> C ("`Tag", x, tag)
    | `Media (x1, m, x2) ->
        C ("`Media", (x1, m, x2), Triple (media_href, media, string)))

and tag : general_tag t =
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
    | `Deprecated x -> C ("`Deprecated", x, general_content)
    | `Param (x1, x2) -> C ("`Param", (x1, x2), Pair (string, general_content))
    | `Raise (x1, x2) ->
        C
          ( "`Raise",
            ((x1 :> general_inline_element), x2),
            Pair (inline_element, general_content) )
    | `Return x -> C ("`Return", x, general_content)
    | `See (x1, x2, x3) ->
        C ("`See", (x1, x2, x3), Triple (url_kind, string, general_content))
    | `Since x -> C ("`Since", x, string)
    | `Before (x1, x2) -> C ("`Before", (x1, x2), Pair (string, general_content))
    | `Version x -> C ("`Version", x, string)
    | `Alert (x1, x2) -> C ("`Alert", (x1, x2), Pair (string, Option string)))

and general_content : general_docs t =
  List (Indirect (ignore_loc, block_element))

let elements : elements t =
  Indirect ((fun x -> (x :> general_docs)), general_content)
let docs =
  Record
    [
      F ("elements", (fun h -> h.elements), elements);
      F ("warnings_tag", (fun h -> h.warnings_tag), Option string);
    ]

let docs_or_stop : docs_or_stop t =
  Variant (function `Docs x -> C ("`Docs", x, docs) | `Stop -> C0 "`Stop")

let inline_element : inline_element Location_.with_location list Type_desc.t =
  List
    (Indirect
       ( (fun x ->
           let x :> general_inline_element Location_.with_location = x in
           ignore_loc x),
         inline_element ))
