type ref_kind = Simple | With_replacement

type alignment = Left | Center | Right

type style = Bold | Italic | Emphasis | Superscript | Subscript
type table_cell_kind = Header | Data

type list_kind = Ordered | Unordered

type uri_kind = URL | File | Document
type math = { start : Loc.point; content : string }

let ast_list_kind : list_kind -> Ast.list_kind = function
  | Ordered -> `Ordered
  | Unordered -> `Unordered

type 'a with_start_point = { start : Loc.point; inner : 'a }
let with_start_point : Lexing.position -> 'a -> 'a with_start_point =
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

(* Token names follow Menhir conventions where ALL_CAPS denote a unit variant,
   in this case generally representing a delimiter *)
type token =
  | Space of string (* ' ' *)
  | Single_newline of string (* '\n' with optional leading horizontal space  *)
  | Blank_line of string
    (* '\n\n' with optional leading horizontal space for both newlines *)
  | Simple_ref of string Loc.with_location with_start_point (* '{!Foo}' *)
  | Ref_with_replacement of
      string Loc.with_location with_start_point (* '{{!Foo} bar}' *)
  | Simple_link of string with_start_point (* '{:janestreet.com}' *)
  | Link_with_replacement of
      string with_start_point (* '{{:janestreet.com}Jane Street}' *)
  | MODULES (* {!modules *)
  | Media of
      (media * media_target) with_start_point (* i.e. '{audio!foo.wav}' *)
  | Media_with_replacement of (media * media_target * string) with_start_point
    (* i.e. '{{audio!foo.wav} presentation on monadic fixpoints}' *)
  | Math_span of string with_start_point (* '{m 2^32-1 }' *)
  | Math_block of string with_start_point (* '{math \\sum_{i=0}^n x^i%}}' *)
  | Code_span of string with_start_point (* '[bind m f]' *)
  | Code_block of code_block with_start_point (* '{@haskell[fix f = f f]}' *)
  | Code_block_with_output of code_block with_start_point
    (* '{@haskell[fix f = f f][Haskell can be cool]}' *)
  | Word of string (* Any whitespace separated word *)
  | Verbatim of string with_start_point (* '{v let foo = bar v}' *)
  | RIGHT_CODE_DELIMITER (* ']}' *)
  | RIGHT_BRACE (* '}' *)
  | Paragraph_style of alignment with_start_point (* i.e. '{L' *)
  | Style of style (* i.e. '{i italic}' or '{_ subscript}' *)
  | List of list_kind (* '{ul' or '{ol'  *)
  | LI (* '{li' *)
  | DASH (* '{-' *)
  | TABLE_LIGHT (* '{t' *)
  | TABLE_HEAVY (* '{table' *)
  | TABLE_ROW (* '{tr' *)
  | Table_cell of Ast.table_cell_kind (* '{td' *)
  | MINUS (* '-' *)
  | PLUS (* '+' *)
  | BAR (* '|' *)
  | Section_heading of (int * string option) with_start_point (* '{2 Foo}' *)
  | Tag of tag (* '@author Fay Carsons' *)
  | Tag_with_content of tag_with_content (* i.e. '@raises Foo' *)
  | Raw_markup of (string option * string) with_start_point
    (* '{% <p>inline html!<\p> %}' *)
  | END (* End of input *)
and tag =
  | Author of string with_start_point (* '@author' *)
  | Since of string with_start_point (* '@since' *)
  | Version of string with_start_point (* '@version' *)
  | Canonical of string with_start_point (* '@canonical' *)
  | INLINE (* '@inline' *)
  | OPEN (* '@open' *)
  | CLOSED (* '@closed' *)
  | HIDDEN (* '@hidden' *)

(* A tag with content is a tag which may be followed by some number of block
   elements delimited by a blank line *)
and tag_with_content =
  | DEPRECATED (* '@deprecated with the release of 1.0.1' *)
  | Before of
      string with_start_point (* '@before 1.0.1 this did something else' *)
  | Raise of string with_start_point (* '@raises Foo' *)
  | Param of string with_start_point (* '@param foo is a monad' *)
  | See of (uri_kind * string) with_start_point
    (* '@see <foo.com> for more info' *)
  | RETURN (* '@return a monad' *)
  | CHILDREN_ORDER (* '@children_order' *)
  | TOC_STATUS (* '@toc_status' *)
  | ORDER_CATEGORY (* '@order_category' *)
  | SHORT_TITLE (* '@short_title' *)
and media =
  | Reference of string Loc.with_location
  | Link of string Loc.with_location
and media_target = Audio | Video | Image

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
  | Tag (Author _) -> "'@author'"
  | Tag_with_content DEPRECATED -> "'@deprecated'"
  | Tag_with_content (Param _) -> "'@param'"
  | Tag_with_content (Raise _) -> "'@raise'"
  | Tag_with_content RETURN -> "'@return'"
  | Tag_with_content (See _) -> "'@see'"
  | Tag_with_content (Before _) -> "'@before'"
  | Tag_with_content CHILDREN_ORDER -> "'@children_order"
  | Tag_with_content TOC_STATUS -> "'@toc_status'"
  | Tag_with_content ORDER_CATEGORY -> "'@order_category'"
  | Tag_with_content SHORT_TITLE -> "@'short_title'"
  | Tag (Since _) -> "'@since'"
  | Tag (Version _) -> "'@version'"
  | Tag (Canonical _) -> "'@canonical'"
  | Tag INLINE -> "'@inline'"
  | Tag OPEN -> "'@open'"
  | Tag CLOSED -> "'@closed'"
  | Tag HIDDEN -> "'@hidden'"
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
  | Tag (Author _) -> "'@author'"
  | Tag_with_content DEPRECATED -> "'@deprecated'"
  | Tag_with_content (Param _) -> "'@param'"
  | Tag_with_content (Raise _) -> "'@raise'"
  | Tag_with_content RETURN -> "'@return'"
  | Tag_with_content (See _) -> "'@see'"
  | Tag_with_content (Before _) -> "'@before'"
  | Tag_with_content CHILDREN_ORDER -> "'@children_order"
  | Tag_with_content TOC_STATUS -> "'@toc_status'"
  | Tag_with_content ORDER_CATEGORY -> "'@order_category'"
  | Tag_with_content SHORT_TITLE -> "@'short_title'"
  | Tag (Since _) -> "'@since'"
  | Tag (Version _) -> "'@version'"
  | Tag (Canonical _) -> "'@canonical'"
  | Tag INLINE -> "'@inline'"
  | Tag OPEN -> "'@open'"
  | Tag CLOSED -> "'@closed'"
  | Tag HIDDEN -> "'@hidden'"

(* Functions and helpers for describing a parsed AST node
   This is useful in error handling inside the Menhir parser *)

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

let dummy_ref inner =
  {
    inner =
      Loc.
        {
          value = inner;
          location = { start = Loc.dummy_pos; end_ = Loc.dummy_pos; file = "" };
        };
    start = Loc.dummy_pos;
  }

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
  | `Reference (`Simple, { value; _ }, _) ->
      describe @@ Simple_ref (dummy_ref value)
  | `Reference (`With_text, { value; _ }, _) ->
      describe @@ Ref_with_replacement (dummy_ref value)

let of_href =
  let open Loc in
  function
  | { value = `Reference value; location } -> Reference { value; location }
  | { value = `Link value; location } -> Link { value; location }

let of_media_kind = function
  | `Audio -> Audio
  | `Image -> Image
  | `Video -> Video

let of_media :
    [> `Media of
       Ast.reference_kind
       * Ast.media_href Loc.with_location
       * string
       * Ast.media ] ->
    token = function
  | `Media (_, href, _, media_kind) ->
      Media
        {
          inner = (of_href href, of_media_kind media_kind);
          start = Loc.dummy_pos;
        }

let describe_nestable_block : Ast.nestable_block_element -> string = function
  | `Paragraph ws -> (
      match ws with
      | Loc.{ value; _ } :: _ -> describe_inline value
      | [] -> describe @@ Word "")
  | `Code_block _ -> describe @@ Code_block empty_code_block
  | `Verbatim _ -> describe @@ Verbatim { inner = ""; start = Loc.dummy_pos }
  | `Modules _ -> describe MODULES
  | `List (ordering, `Light, _) ->
      describe @@ if ordering = `Ordered then PLUS else MINUS
  | `List (ordering, `Heavy, _) ->
      describe @@ List (if ordering = `Ordered then Ordered else Unordered)
  | `Table (_, kind) ->
      describe @@ if kind = `Light then TABLE_LIGHT else TABLE_HEAVY
  | `Math_block _ ->
      describe @@ Math_block { start = Loc.dummy_pos; inner = "" }
  | `Media _ as media -> describe @@ of_media media

let of_ast_ref : [ `Document | `File | `Url ] -> uri_kind = function
  | `Document -> Document
  | `File -> File
  | `Url -> URL

let describe_tag : Ast.tag -> string = function
  | `See (kind, _, _) ->
      describe
      @@ Tag_with_content
           (See { inner = (of_ast_ref kind, ""); start = Loc.dummy_pos })
  | `Author inner -> describe @@ Tag (Author { inner; start = Loc.dummy_pos })
  | `Deprecated _ -> describe @@ Tag_with_content DEPRECATED
  | `Param (inner, _) ->
      describe @@ Tag_with_content (Param { inner; start = Loc.dummy_pos })
  | `Raise (inner, _) ->
      describe @@ Tag_with_content (Raise { inner; start = Loc.dummy_pos })
  | `Return _ -> describe @@ Tag_with_content RETURN
  | `Since inner -> describe @@ Tag (Since { inner; start = Loc.dummy_pos })
  | `Before (inner, _) ->
      describe @@ Tag_with_content (Before { inner; start = Loc.dummy_pos })
  | `Version inner -> describe @@ Tag (Version { inner; start = Loc.dummy_pos })
  | `Closed -> describe @@ Tag CLOSED
  | `Open -> describe @@ Tag OPEN
  | `Canonical Loc.{ value = inner; _ } ->
      describe @@ Tag (Canonical { inner; start = Loc.dummy_pos })
  | `Hidden -> describe @@ Tag HIDDEN
  | `Inline -> describe @@ Tag INLINE
  | `Children_order _ -> describe @@ Tag_with_content CHILDREN_ORDER
  | `Toc_status _ -> describe @@ Tag_with_content TOC_STATUS
  | `Order_category _ -> describe @@ Tag_with_content ORDER_CATEGORY
  | `Short_title _ -> describe @@ Tag_with_content SHORT_TITLE
