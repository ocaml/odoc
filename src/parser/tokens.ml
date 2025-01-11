type ref_kind = Simple | With_replacement

type media = Reference of string | Link of string
type media_target = Audio | Video | Image

type alignment = Left | Center | Right

type style = Bold | Italic | Emphasis | Superscript | Subscript
type table_cell_kind = Header | Data

type list_kind = Ordered | Unordered

type internal_reference = URL | File | Document
type math = { start : Loc.point; content : string }

let ast_list_kind : list_kind -> Ast.list_kind = function
  | Ordered -> `Ordered
  | Unordered -> `Unordered

type 'a with_start_pos = { start : Loc.point; inner : 'a }
let with_start_pos : Lexing.position -> 'a -> 'a with_start_pos =
 fun start inner -> { start = Loc.make_point start; inner }

type code_block = {
  metadata : meta option;
  delimiter : string option;
  content : string Loc.with_location;
}
and meta = {
  language_tag : string Loc.with_location;
  tags : string Loc.with_location option;
}

type token =
  | Space of string
  | Single_newline of string
  | Blank_line of string
  | Simple_ref of string with_start_pos
  | Ref_with_replacement of string with_start_pos
  | Simple_link of string with_start_pos
  | Link_with_replacement of string with_start_pos
  | MODULES
  | Media of (media * media_target) with_start_pos
  | Media_with_replacement of (media * media_target * string) with_start_pos
  (* Start location *)
  | Math_span of string with_start_pos
  | Math_block of string with_start_pos
  | Code_span of string with_start_pos
  | Code_block of code_block with_start_pos
  | Code_block_with_output of code_block with_start_pos
  | Word of string
  | Verbatim of string with_start_pos
  | RIGHT_CODE_DELIMITER
  | RIGHT_BRACE
  | Paragraph_style of alignment with_start_pos
  | Style of style
  | List of list_kind
  | LI
  | DASH
  | TABLE_LIGHT
  | TABLE_HEAVY
  | TABLE_ROW
  | Table_cell of Ast.table_cell_kind
  | MINUS
  | PLUS
  | BAR
  | Section_heading of (int * string option) with_start_pos
  | Author of string with_start_pos
  | DEPRECATED
  | Param of string with_start_pos
  | Raise of string with_start_pos
  | RETURN
  | See of (internal_reference * string) with_start_pos
  | Since of string with_start_pos
  | Before of string with_start_pos
  | Version of string with_start_pos
  | Canonical of string with_start_pos
  | INLINE
  | OPEN
  | CLOSED
  | HIDDEN
  | Raw_markup of (string option * string) with_start_pos
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
  | Space _ -> "\t"
  | Single_newline _ -> "\n"
  | Blank_line _ -> "\n\n"
  | Simple_ref _ -> "{!"
  | Ref_with_replacement _ -> "{{!"
  | Simple_link _ -> "{:"
  | Link_with_replacement _ -> "{{:"
  | MODULES -> "{!modules:"
  | Media { inner = ref_kind, media_kind; _ } ->
      let ref_kind, media_kind = media_description ref_kind media_kind in
      Printf.sprintf "{%s%s" media_kind ref_kind
  | Media_with_replacement { inner = ref_kind, media_kind, _; _ } ->
      let ref_kind, media_kind = media_description ref_kind media_kind in
      Printf.sprintf "{{%s%s" media_kind ref_kind
  | Math_span _ -> "{m"
  | Math_block _ -> "{math"
  | Code_span _ -> "["
  | Code_block _ | Code_block_with_output _ -> "{["
  | Word w -> w
  | Verbatim _ -> "{v"
  | RIGHT_CODE_DELIMITER -> "]}"
  | RIGHT_BRACE -> "}"
  | Paragraph_style { inner = Left; _ } -> "'{L'"
  | Paragraph_style { inner = Center; _ } -> "'{C'"
  | Paragraph_style { inner = Right; _ } -> "'{R'"
  | Style Bold -> "'{b'"
  | Style Italic -> "'{i'"
  | Style Emphasis -> "'{e'"
  | Style Superscript -> "'{^'"
  | Style Subscript -> "'{_'"
  | List Ordered -> "{ol"
  | List Unordered -> "{ul"
  | LI -> "'{li ...}'"
  | DASH -> "'{- ...}'"
  | TABLE_LIGHT -> "{t"
  | TABLE_HEAVY -> "{table"
  | TABLE_ROW -> "'{tr'"
  | Table_cell `Header -> "'{th'"
  | Table_cell `Data -> "'{td'"
  | MINUS -> "'-'"
  | PLUS -> "'+'"
  | BAR -> "'|'"
  | Section_heading { inner = level, label; _ } ->
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
  | Raw_markup { inner = None, _; _ } -> "'{%...%}'"
  | Raw_markup { inner = Some target, _; _ } -> "'{%" ^ target ^ ":...%}'"
  | END -> "EOI"

(* [`Minus] and [`Plus] are interpreted as if they start list items. Therefore,
   for error messages based on [Token.describe] to be accurate, formatted
   [`Minus] and [`Plus] should always be plausibly list item bullets. *)
let describe : token -> string = function
  | Space _ -> "(horizontal space)"
  | Media { inner = ref_kind, media_kind; _ } ->
      let ref_kind, media_kind = media_description ref_kind media_kind in
      Printf.sprintf "{%s%s" media_kind ref_kind
  | Media_with_replacement { inner = ref_kind, media_kind, _; _ } ->
      let ref_kind, media_kind = media_description ref_kind media_kind in
      Printf.sprintf "{{%s%s" media_kind ref_kind
  | Word w -> Printf.sprintf "'%s'" w
  | Code_span _ -> "'[...]' (code)"
  | Raw_markup _ -> "'{%...%}' (raw markup)"
  | Paragraph_style { inner = Left; _ } -> "'{L ...}' (left alignment)"
  | Paragraph_style { inner = Center; _ } -> "'{C ...}' (center alignment)"
  | Paragraph_style { inner = Right; _ } -> "'{R ...}' (right alignment)"
  | Style Bold -> "'{b ...}' (boldface text)"
  | Style Italic -> "'{i ...}' (italic text)"
  | Style Emphasis -> "'{e ...}' (emphasized text)"
  | Style Superscript -> "'{^...}' (superscript)"
  | Style Subscript -> "'{_...}' (subscript)"
  | Math_span _ -> "'{m ...}' (math span)"
  | Math_block _ -> "'{math ...}' (math block)"
  | Simple_ref _ -> "'{!...}' (cross-reference)"
  | Ref_with_replacement _ -> "'{{!...} ...}' (cross-reference)"
  | Simple_link _ -> "'{:...} (external link)'"
  | Link_with_replacement _ -> "'{{:...} ...}' (external link)"
  | END -> "end of text"
  | Single_newline _ -> "newline"
  | Blank_line _ -> "blank line"
  | RIGHT_BRACE -> "'}'"
  | RIGHT_CODE_DELIMITER -> "']}'"
  | Code_block _ | Code_block_with_output _ -> "'{[...]}' (code block)"
  | Verbatim _ -> "'{v ... v}' (verbatim text)"
  | MODULES -> "'{!modules ...}'"
  | List Unordered -> "'{ul ...}' (bulleted list)"
  | List Ordered -> "'{ol ...}' (numbered list)"
  | LI -> "'{li ...}' (list item)"
  | DASH -> "'{- ...}' (list item)"
  | TABLE_LIGHT -> "'{t ...}' (table)"
  | TABLE_HEAVY -> "'{table ...}' (table)"
  | TABLE_ROW -> "'{tr ...}' (table row)"
  | Table_cell `Header -> "'{th ... }' (table header cell)"
  | Table_cell `Data -> "'{td ... }' (table data cell)"
  | MINUS -> "'-' (bulleted list item)"
  | PLUS -> "'+' (numbered list item)"
  | BAR -> "'|'"
  | Section_heading { inner = level, _; _ } ->
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

let empty_code_block =
  {
    inner =
      {
        metadata = None;
        delimiter = None;
        content =
          Loc.at
            Loc.{ start = Loc.dummy_pos; end_ = Loc.dummy_pos; file = "" }
            "";
      };
    start = Loc.dummy_pos;
  }

let of_ast_style : Ast.style -> style = function
  | `Bold -> Bold
  | `Italic -> Italic
  | `Emphasis -> Emphasis
  | `Superscript -> Superscript
  | `Subscript -> Subscript

let to_ast_style : style -> Ast.style = function
  | Bold -> `Bold
  | Italic -> `Italic
  | Emphasis -> `Emphasis
  | Superscript -> `Superscript
  | Subscript -> `Subscript

let describe_inline : Ast.inline_element -> string = function
  | `Word w -> describe @@ Word w
  | `Space _ -> describe @@ Space ""
  | `Styled (style, _) -> describe @@ Style (of_ast_style style)
  | `Code_span _ -> describe @@ Code_span { inner = ""; start = Loc.dummy_pos }
  | `Math_span _ -> describe @@ Math_span { start = Loc.dummy_pos; inner = "" }
  | `Raw_markup inner -> describe @@ Raw_markup { inner; start = Loc.dummy_pos }
  | `Link (inner, []) ->
      describe @@ Simple_link { inner; start = Loc.dummy_pos }
  | `Link (inner, _ :: _) ->
      describe @@ Link_with_replacement { inner; start = Loc.dummy_pos }
  | `Reference (`Simple, { value = inner; _ }, _) ->
      describe @@ Simple_ref { inner; start = Loc.dummy_pos }
  | `Reference (`With_text, { value = inner; _ }, _) ->
      describe @@ Ref_with_replacement { inner; start = Loc.dummy_pos }

let of_href = function `Reference s -> Reference s | `Link s -> Link s

let of_media_kind = function
  | `Audio -> Audio
  | `Image -> Image
  | `Video -> Video

let of_media = function
  | `Media (_, Loc.{ value; _ }, _, kind) ->
      Media
        { inner = (of_href value, of_media_kind kind); start = Loc.dummy_pos }

(* NOTE: Fix list *)
let describe_nestable_block : Ast.nestable_block_element -> string = function
  | `Paragraph ws -> (
      match ws with
      | Loc.{ value; _ } :: _ -> describe_inline value
      | [] -> describe @@ Word "")
  | `Code_block _ -> describe @@ Code_block empty_code_block
  | `Verbatim _ -> describe @@ Verbatim { inner = ""; start = Loc.dummy_pos }
  | `Modules _ -> describe MODULES
  | `List (_, _, _) -> "List"
  | `Table (_, kind) ->
      describe @@ if kind = `Light then TABLE_LIGHT else TABLE_HEAVY
  | `Math_block _ ->
      describe @@ Math_block { start = Loc.dummy_pos; inner = "" }
  | `Media _ as media -> describe @@ of_media media

let of_ast_ref : [ `Document | `File | `Url ] -> internal_reference = function
  | `Document -> Document
  | `File -> File
  | `Url -> URL

let to_ast_ref : internal_reference -> [ `Url | `File | `Document ] = function
  | URL -> `Url
  | File -> `File
  | Document -> `Document

let describe_tag : Ast.tag -> string = function
  | `See (kind, _, _) ->
      describe @@ See { inner = (of_ast_ref kind, ""); start = Loc.dummy_pos }
  | `Author inner -> describe @@ Author { inner; start = Loc.dummy_pos }
  | `Deprecated _ -> describe DEPRECATED
  | `Param (inner, _) -> describe @@ Param { inner; start = Loc.dummy_pos }
  | `Raise (inner, _) -> describe @@ Raise { inner; start = Loc.dummy_pos }
  | `Return _ -> describe RETURN
  | `Since inner -> describe @@ Since { inner; start = Loc.dummy_pos }
  | `Before (inner, _) -> describe @@ Before { inner; start = Loc.dummy_pos }
  | `Version inner -> describe @@ Version { inner; start = Loc.dummy_pos }
  | `Closed -> describe CLOSED
  | `Open -> describe OPEN
  | `Canonical Loc.{ value = inner; _ } ->
      describe @@ Canonical { inner; start = Loc.dummy_pos }
  | `Hidden -> describe HIDDEN
  | `Inline -> describe INLINE
