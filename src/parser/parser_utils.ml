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

(* NOTE : (@faycarsons) Should this be in Ast.ml? This takes an Ast.t no? *)
let describe_element = function
  | `Reference (`Simple, _, _) -> describe (Simple_ref "")
  | `Reference (`With_text, _, _) ->
      describe (Ref_with_replacement "")
  | `Link _ -> describe (Link_with_replacement "")
  | `Heading (level, _, _) -> describe (Section_heading (level, None))
