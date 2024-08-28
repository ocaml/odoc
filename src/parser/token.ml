(* NOTE : (@faycarsons) keep as reference for the moment.
   Should probably become utilities for Menhir-defined tokens *)

(* This module contains the token type, emitted by the lexer, and consumed by
   the comment syntax parser. It also contains two functions that format tokens
   for error messages. *)

type section_heading = [ `Begin_section_heading of int * string option ]
type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]
type paragraph_style = [ `Left | `Center | `Right ]


type tag =
  [ `Tag of
    [ `Author of string
    | `Deprecated
    | `Param of string
    | `Raise of string
    | `Return
    | `See of [ `Url | `File | `Document ] * string
    | `Since of string
    | `Before of string
    | `Version of string
    | `Canonical of string
    | `Inline
    | `Open
    | `Closed
    | `Hidden ] ]

type media = [ `Audio | `Video | `Image ]
type media_href = [ `Reference of string | `Link of string ]

type media_markup =
  [ `Simple_media of media_href * media
  | `Media_with_replacement_text of media_href * media * string ]

let s_of_media kind media =
  match (kind, media) with
  | `Simple, `Audio -> "{audio!"
  | `Simple, `Video -> "{video!"
  | `Simple, `Image -> "{image!"
  | `Replaced, `Audio -> "{{audio!"
  | `Replaced, `Video -> "{{video!"
  | `Replaced, `Image -> "{{image!"

type t =
  [ (* End of input. *)
    `End
  | (* Runs of whitespace. [Blank_line] is any run of whitespace that contains two
       or more newline characters. [Single_newline] is any run of whitespace that
       contains exactly one newline character. [Space] is any run of whitespace
       that contains no newline characters.

       It is an important invariant in the parser that no adjacent whitespace
       tokens are emitted by the lexer. Otherwise, there would be the need for
       unbounded lookahead, a (co-?)ambiguity between
       [Single_newline Single_newline] and [Blank_line], and other problems. *)
    `Space of
    string
  | `Single_newline of string
  | `Blank_line of string
  | (* A right curly brace ([}]), i.e. end of markup. *)
    `Right_brace
  | `Right_code_delimiter
  | (* Words are anything that is not whitespace or markup. Markup symbols can be
       be part of words if escaped.

       Words can contain plus and minus symbols, but those are emitted as [Plus]
       and [Minus] tokens. The parser combines plus and minus into words, except
       when they appear first on a line, in which case the tokens are list item
       bullets. *)
    `Word of
    string
  | `Code_span of string
  | `Raw_markup of string option * string
  | `Math_span of string
  | `Math_block of string
  | `Begin_style of style
  | `Begin_paragraph_style of paragraph_style
  | (* Other inline element markup. *)
    `Simple_reference of string
  | `Begin_reference_with_replacement_text of string
  | `Simple_link of string
  | `Begin_link_with_replacement_text of string
  | media_markup
  | (* Leaf block element markup. *)
    `Code_block of
    (string Loc.with_location * string Loc.with_location option) option
    * string
    * string Loc.with_location
    * bool
  | `Verbatim of string
  | `Modules of string
  | (* List markup. *)
    `Begin_list of [ `Unordered | `Ordered ]
  | `Begin_list_item of [ `Li | `Dash ]
  | (* Table markup. *)
    `Begin_table_light
  | `Begin_table_heavy
  | `Begin_table_row
  | `Begin_table_cell of [ `Header | `Data ]
  | `Minus
  | `Plus
  | `Bar
  | section_heading
  | tag ]
