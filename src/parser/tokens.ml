type ref_kind = Simple | With_replacement

type media = Reference of string | Link of string
type media_target = Audio | Video | Image

type paragraph_style = [ `Left | `Center | `Right ]

type style = [ `Bold | `Italic | `Emphasis | `Superscript | `Subscript ]
type table_cell_kind = [ `Header | `Data ]

type token =
  | SPACE
  | Space of string
  | NEWLINE
  | Single_newline of string
  | Blank_line of string
  | Simple_ref of string
  | Ref_with_replacement of string
  | Simple_link of string
  | Link_with_replacement of string
  | Modules of string
  | Media of (media * media_target)
  | Media_with_replacement of (media * media_target * string)
  | Math_span of string
  | Math_block of string
  | Code_span of string
  | Code_block of Ast.code_block
  | Word of string
  | Verbatim of string
  | RIGHT_CODE_DELIMITER
  | RIGHT_BRACE
  | Paragraph_style of paragraph_style
  | Style of style
  | List of Ast.list_kind
  | List_item of Ast.list_item
  | TABLE_LIGHT
  | TABLE_HEAVY
  | TABLE_ROW
  | Table_cell of Ast.table_cell_kind
  | MINUS
  | PLUS
  | BAR
  | Section_heading of (int * string option)
  | Author of string
  | DEPRECATED
  | Param of string
  | Raise of string
  | RETURN
  | See of ([ `Url | `File | `Document ] * string)
  | Since of string
  | Before of string
  | Version of string
  | Canonical of string
  | INLINE
  | OPEN
  | CLOSED
  | HIDDEN
  | Raw_markup of (string option * string)
  | END

let media_description ref_kind media_kind =
  let media_kind =
    match media_kind with
    | Audio -> "audio"
    | Video -> "video"
    | Image -> "image"
  in
  let ref_kind = match ref_kind with Reference _ -> "!" | Link _ -> ":" in
  (ref_kind, media_kind)

let print : token -> string = function
  | SPACE | Space _ -> "\t"
  | NEWLINE | Single_newline _ -> "\n"
  | Blank_line _ -> "\n\n"
  | Simple_ref _ -> "{!"
  | Ref_with_replacement _ -> "{{!"
  | Simple_link _ -> "{:"
  | Link_with_replacement _ -> "{{:"
  | Modules _ -> "{!modules:"
  | Media (ref_kind, media_kind) ->
      let ref_kind, media_kind = media_description ref_kind media_kind in
      Printf.sprintf "{%s%s" media_kind ref_kind
  | Media_with_replacement (ref_kind, media_kind, _) ->
      let ref_kind, media_kind = media_description ref_kind media_kind in
      Printf.sprintf "{{%s%s" media_kind ref_kind
  | Math_span _ -> "{m"
  | Math_block _ -> "{math"
  | Code_span _ -> "["
  | Code_block _ -> "{["
  | Word w -> w
  | Verbatim _ -> "{v"
  | RIGHT_CODE_DELIMITER -> "]}"
  | RIGHT_BRACE -> "}"
  | Paragraph_style `Left -> "'{L'"
  | Paragraph_style `Center -> "'{C'"
  | Paragraph_style `Right -> "'{R'"
  | Style `Bold -> "'{b'"
  | Style `Italic -> "'{i'"
  | Style `Emphasis -> "'{e'"
  | Style `Superscript -> "'{^'"
  | Style `Subscript -> "'{_'"
  | List `Ordered -> "{ol"
  | List `Unordered -> "{ul"
  | List_item `Li -> "'{li ...}'"
  | List_item `Dash -> "'{- ...}'"
  | TABLE_LIGHT -> "{t"
  | TABLE_HEAVY -> "{table"
  | TABLE_ROW -> "'{tr'"
  | Table_cell `Header -> "'{th'"
  | Table_cell `Data -> "'{td'"
  | MINUS -> "'-'"
  | PLUS -> "'+'"
  | BAR -> "'|'"
  | Section_heading (level, label) ->
      let label = match label with None -> "" | Some label -> ":" ^ label in
      Printf.sprintf "'{%i%s'" level label
  | Author _ -> "'@author'"
  | DEPRECATED -> "'@deprecated'"
  | Param _ -> "'@param'"
  | Raise _ -> "'@raise'"
  | RETURN -> "'@return'"
  | See _ -> "'@see'"
  | Since _ -> "'@since'"
  | Before _ -> "'@before'"
  | Version _ -> "'@version'"
  | Canonical _ -> "'@canonical'"
  | INLINE -> "'@inline'"
  | OPEN -> "'@open'"
  | CLOSED -> "'@closed'"
  | HIDDEN -> "'@hidden"
  | Raw_markup (None, _) -> "'{%...%}'"
  | Raw_markup (Some target, _) -> "'{%" ^ target ^ ":...%}'"
  | END -> "EOI"

(* [`Minus] and [`Plus] are interpreted as if they start list items. Therefore,
   for error messages based on [Token.describe] to be accurate, formatted
   [`Minus] and [`Plus] should always be plausibly list item bullets. *)
let describe : token -> string = function
  | Space _ -> "(horizontal space)"
  | Media (ref_kind, media_kind) ->
      let ref_kind, media_kind = media_description ref_kind media_kind in
      Printf.sprintf "{%s%s" media_kind ref_kind
  | Media_with_replacement (ref_kind, media_kind, _) ->
      let ref_kind, media_kind = media_description ref_kind media_kind in
      Printf.sprintf "{{%s%s" media_kind ref_kind
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
  | Ref_with_replacement _ -> "'{{!...} ...}' (cross-reference)"
  | Simple_link _ -> "'{:...} (external link)'"
  | Link_with_replacement _ -> "'{{:...} ...}' (external link)"
  | END -> "end of text"
  | SPACE -> "whitespace"
  | Single_newline _ | NEWLINE -> "line break"
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
  | TABLE_LIGHT -> "'{t ...}' (table)"
  | TABLE_HEAVY -> "'{table ...}' (table)"
  | TABLE_ROW -> "'{tr ...}' (table row)"
  | Table_cell `Header -> "'{th ... }' (table header cell)"
  | Table_cell `Data -> "'{td ... }' (table data cell)"
  | MINUS -> "'-' (bulleted list item)"
  | PLUS -> "'+' (numbered list item)"
  | BAR -> "'|'"
  | Section_heading (level, _) ->
      Printf.sprintf "'{%i ...}' (section heading)" level
  | Author _ -> "'@author'"
  | DEPRECATED -> "'@deprecated'"
  | Param _ -> "'@param'"
  | Raise _ -> "'@raise'"
  | RETURN -> "'@return'"
  | See _ -> "'@see'"
  | Since _ -> "'@since'"
  | Before _ -> "'@before'"
  | Version _ -> "'@version'"
  | Canonical _ -> "'@canonical'"
  | INLINE -> "'@inline'"
  | OPEN -> "'@open'"
  | CLOSED -> "'@closed'"
  | HIDDEN -> "'@hidden"
