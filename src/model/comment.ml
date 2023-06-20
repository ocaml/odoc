module Path = Paths.Path
module Reference = Paths.Reference
module Identifier = Paths.Identifier

type 'a with_location = 'a Location_.with_location

type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]

type alignment = [ `Left | `Center | `Right ]

type raw_markup_target = string

type leaf_inline_element =
  [ `Space
  | `Word of string
  | `Code_span of string
  | `Math_span of string
  | `Raw_markup of raw_markup_target * string ]

type non_link_inline_element =
  [ leaf_inline_element
  | `Styled of style * non_link_inline_element with_location list ]

(* The cross-referencer stores section heading text, and sometimes pastes it
   into link contents. This type alias is provided for use by the
   cross-referencer. *)
type link_content = non_link_inline_element with_location list

type reference_element = [ `Reference of Reference.t * link_content ]

type inline_element =
  [ leaf_inline_element
  | `Styled of style * inline_element with_location list
  | reference_element
  | `Link of string * link_content ]

type paragraph = inline_element with_location list

type module_reference = {
  module_reference : Reference.Module.t;
  module_synopsis : paragraph option;
}
(** The [{!modules: ...}] markup. [module_synopsis] is initially [None], it is
    resolved during linking. *)

type 'a cell = 'a with_location list * [ `Header | `Data ]
type 'a row = 'a cell list
type 'a grid = 'a row list

type 'a abstract_table = {
  data : 'a grid;
  align : alignment option list option;
}

type nestable_block_element =
  [ `Paragraph of paragraph
  | `Code_block of
    string option
    * string with_location
    * nestable_block_element with_location list option
  | `Math_block of string
  | `Verbatim of string
  | `Modules of module_reference list
  | `Table of nestable_block_element abstract_table
  | `List of
    [ `Unordered | `Ordered ] * nestable_block_element with_location list list
  ]

type tag =
  [ `Author of string
  | `Deprecated of nestable_block_element with_location list
  | `Param of string * nestable_block_element with_location list
  | `Raise of
    [ `Code_span of string | reference_element ]
    * nestable_block_element with_location list
  | `Return of nestable_block_element with_location list
  | `See of
    [ `Url | `File | `Document ]
    * string
    * nestable_block_element with_location list
  | `Since of string
  | `Before of string * nestable_block_element with_location list
  | `Version of string
  | `Alert of string * string option ]

type heading_level =
  [ `Title
  | `Section
  | `Subsection
  | `Subsubsection
  | `Paragraph
  | `Subparagraph ]

type attached_block_element = [ nestable_block_element | `Tag of tag ]

type heading_attrs = {
  heading_level : heading_level;
  heading_label_explicit : bool;
      (** Whether the label have been written by the user. *)
}

type block_element =
  [ nestable_block_element
  | `Heading of
    heading_attrs * Identifier.Label.t * inline_element with_location list
  | `Tag of tag ]

type docs = block_element with_location list

type docs_or_stop = [ `Docs of docs | `Stop ]

(** The synopsis is the first element of a comment if it is a paragraph.
    Otherwise, there is no synopsis. *)
let synopsis = function
  | { Location_.value = `Paragraph p; _ } :: _ -> Some p
  | _ -> None

let rec link_content_of_inline_element :
    inline_element with_location -> link_content =
 fun x ->
  let v = x.Location_.value in
  match v with
  | #leaf_inline_element as e -> [ { x with value = e } ]
  | `Reference (_, r) -> r
  | `Link (_, l) -> l
  | `Styled (st, elems) ->
      [ { x with value = `Styled (st, link_content_of_inline_elements elems) } ]

and link_content_of_inline_elements l =
  l |> List.map link_content_of_inline_element |> List.concat
