module Path = Paths.Path
module Reference = Paths.Reference
module Identifier = Paths.Identifier



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
  | `Styled of style * non_link_inline_element list
]

type inline_element = [
  | non_link_inline_element
  | `Reference of Reference.any * non_link_inline_element list
  | `Link of string * non_link_inline_element list
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
  | `Heading of
      heading_level * Identifier.label option * non_link_inline_element list
  | `Tag of tag
]

type docs = block_element list

type docs_or_stop = [
  | `Docs of docs
  | `Stop
]
