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

open Parser



let[@warning "-8"] print : Parser.token -> string = function
  | Paragraph_style `Left -> "'{L'"
  | Paragraph_style `Center -> "'{C'"
  | Paragraph_style `Right -> "'{R'"
  | Style `Bold -> "'{b'"
  | Style `Italic -> "'{i'"
  | Style `Emphasis -> "'{e'"
  | Style `Superscript -> "'{^'"
  | Style `Subscript -> "'{_'"
  | Ref_with_replacement _ -> "'{{!'"
  | Link_with_replacement _ -> "'{{:'"
  | List_item `Li -> "'{li ...}'"
  | List_item `Dash -> "'{- ...}'"
  | Table_light -> "{t"
  | Table_heavy -> "{table"
  | Table_row -> "'{tr'"
  | Table_cell `Header -> "'{th'"
  | Table_cell `Data -> "'{td'"
  | MINUS -> "'-'"
  | PLUS -> "'+'"
  | BAR -> "'|'"
  | Section_heading (level, label) ->
      let label = match label with None -> "" | Some label -> ":" ^ label in
      Printf.sprintf "'{%i%s'" level label
  | Tag (Author _) -> "'@author'"
  | Tag Deprecated -> "'@deprecated'"
  | Tag (Param _) -> "'@param'"
  | Tag (Raise _) -> "'@raise'"
  | Tag Return -> "'@return'"
  | Tag (See _) -> "'@see'"
  | Tag (Since _) -> "'@since'"
  | Tag (Before _) -> "'@before'"
  | Tag (Version _) -> "'@version'"
  | Tag (Canonical _) -> "'@canonical'"
  | Tag Inline -> "'@inline'"
  | Tag Open -> "'@open'"
  | Tag Closed -> "'@closed'"
  | Tag Hidden -> "'@hidden"
  | Raw_markup (None, _) -> "'{%...%}'"
  | Raw_markup (Some target, _) -> "'{%" ^ target ^ ":...%}'"

(* [`Minus] and [`Plus] are interpreted as if they start list items. Therefore,
   for error messages based on [Token.describe] to be accurate, formatted
   [`Minus] and [`Plus] should always be plausibly list item bullets. *)
let[@warning "-8"] describe : Parser.token -> string = function
  | Word w -> Printf.sprintf "'%s'" w
  | Code_span _ -> "'[...]' (code)"
  | Raw_markup _ -> "'{%...%}' (raw markup)"
  | Paragraph_style `Left -> "'{L ...}' (left alignment)"
  | Paragraph_style `Center -> "'{C ...}' (center alignment)"
  | Paragraph_style `Right -> "'{R ...}' (right alignment)"
  | Style `Bold -> "'{b ...}' (boldface text)"
  | Style `Italic -> "'{i ...}' (italic text)"
  | Style `Emphasis -> "'{e ...}' (emphasized text)"
  | Style `Superscript -> "'{^...}' (superscript)"
  | Style `Subscript -> "'{_...}' (subscript)"
  | Math_span _ -> "'{m ...}' (math span)"
  | Math_block _ -> "'{math ...}' (math block)"
  | Simple_ref _ -> "'{!...}' (cross-reference)"
  | Ref_with_replacement _ ->
      "'{{!...} ...}' (cross-reference)"
  | Simple_link _ -> "'{:...} (external link)'"
  | Link_with_replacement _ -> "'{{:...} ...}' (external link)"
  | END -> "end of text"
  | SPACE -> "whitespace"
  | Single_newline _ -> "line break"
  | Blank_line _ -> "blank line"
  | RIGHT_BRACE -> "'}'"
  | RIGHT_CODE_DELIMITER -> "']}'"
  | Code_block _ -> "'{[...]}' (code block)"
  | Verbatim _ -> "'{v ... v}' (verbatim text)"
  | Modules _ -> "'{!modules ...}'"
  | List `Unordered -> "'{ul ...}' (bulleted list)"
  | List `Ordered -> "'{ol ...}' (numbered list)"
  | List_item `Li -> "'{li ...}' (list item)"
  | List_item `Dash -> "'{- ...}' (list item)"
  | Table_light -> "'{t ...}' (table)"
  | Table_heavy -> "'{table ...}' (table)"
  | Table_row -> "'{tr ...}' (table row)"
  | Table_cell `Header -> "'{th ... }' (table header cell)"
  | Table_cell `Data -> "'{td ... }' (table data cell)"
  | MINUS -> "'-' (bulleted list item)"
  | PLUS -> "'+' (numbered list item)"
  | BAR -> "'|'"
  | Section_heading (level, _) ->
      Printf.sprintf "'{%i ...}' (section heading)" level
  | Tag (Author _) -> "'@author'"
  | Tag Deprecated -> "'@deprecated'"
  | Tag (Param _) -> "'@param'"
  | Tag (Raise _) -> "'@raise'"
  | Tag ( Return ) -> "'@return'"
  | Tag (See _) -> "'@see'"
  | Tag (Since _) -> "'@since'"
  | Tag (Before _) -> "'@before'"
  | Tag (Version _) -> "'@version'"
  | Tag (Canonical _) -> "'@canonical'"
  | Tag Inline -> "'@inline'"
  | Tag Open -> "'@open'"
  | Tag Closed -> "'@closed'"
  | Tag Hidden -> "'@hidden"
  | COMMENT -> "top-level text"

let describe_element = function
  | `Reference (`Simple, _, _) -> describe (Simple_ref "")
  | `Reference (`With_text, _, _) ->
      describe (Ref_with_replacement "")
  | `Link _ -> describe (Link_with_replacement "")
  | `Heading (level, _, _) -> describe (Section_heading (level, None))
