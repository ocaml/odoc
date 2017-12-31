module Path = Paths.Path
module Reference = Paths.Reference
module Identifier = Paths.Identifier

type 'a with_location = 'a Location_.with_location



type style = [
  | `Bold
  | `Italic
  | `Emphasis
  | `Superscript
  | `Subscript
]

type non_link_inline_element = [
  | `Space
  | `Word of string
  | `Code_span of string
  | `Styled of style * (non_link_inline_element with_location) list
]

(* The cross-referencer stores section heading text, and sometimes pastes it
   into link contents. This type alias is provided for use by the
   cross-referencer. *)
type link_content = (non_link_inline_element with_location) list

type inline_element = [
  | non_link_inline_element
  | `Reference of Reference.any * link_content
  | `Link of string * link_content
]

type nestable_block_element = [
  | `Paragraph of inline_element list
  | `Code_block of string
  | `Verbatim of string
  | `Modules of Reference.module_ list
  | `List of [ `Unordered | `Ordered ] * (nestable_block_element list) list
]

type tag = [
  | `Author of string
  | `Deprecated of nestable_block_element list
  | `Param of string * nestable_block_element list
  | `Raise of string * nestable_block_element list
  | `Return of nestable_block_element list
  | `See of [ `Url | `File | `Document ] * string * nestable_block_element list
  | `Since of string
  | `Before of string * nestable_block_element list
  | `Version of string
  | `Canonical of Path.module_ * Reference.module_
]

type heading_level = [
  | `Title
  | `Section
  | `Subsection
  | `Subsubsection
]

type block_element = [
  | nestable_block_element
  | `Heading of heading_level * Identifier.label option * link_content
  | `Tag of tag
]

type docs = block_element list

type docs_or_stop = [
  | `Docs of docs
  | `Stop
]
