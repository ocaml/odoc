{

(* This is used for code and verbatim blocks. It can be done with a regular
   expression, but the regexp gets quite ugly, so a function is easier to
   understand. *)
let trim_leading_blank_lines : string -> string = fun s ->
  let rec scan_for_last_newline : int -> int -> int =
      fun index trim_until ->
    if index >= String.length s then
      String.length s
    else
      match s.[index] with
      | ' ' | '\t' | '\r' -> scan_for_last_newline (index + 1) trim_until
      | '\n' -> scan_for_last_newline (index + 1) (index + 1)
      | _ -> trim_until
  in
  let trim_until = scan_for_last_newline 0 0 in
  String.sub s trim_until (String.length s - trim_until)

let trim_trailing_blank_lines : string -> string = fun s ->
  let rec scan_for_last_newline : int -> int option -> int option =
      fun index trim_from ->
    if index < 0 then
      Some 0
    else
      match s.[index] with
      | ' ' | '\t' | '\r' -> scan_for_last_newline (index - 1) trim_from
      | '\n' -> scan_for_last_newline (index - 1) (Some index)
      | _ -> trim_from
  in
  let last = String.length s - 1 in
  match scan_for_last_newline last None with
  | None ->
    s
  | Some trim_from ->
    let trim_from =
      if trim_from > 0 && s.[trim_from - 1] = '\r' then
        trim_from - 1
      else
        trim_from
    in
    String.sub s 0 trim_from

let trim_leading_space_or_accept_whitespace t lexbuf =
  match t.[0] with
  | ' ' -> String.sub t 1 (String.length t - 1)
  | '\t' | '\r' | '\n' -> t
  | _ -> Helpers.no_leading_whitespace_in_verbatim (Lexing.lexeme_start lexbuf)
  | exception Invalid_argument _ -> ""

let trim_trailing_space_or_accept_whitespace t lexbuf =
  match t.[String.length t - 1] with
  | ' ' -> String.sub t 0 (String.length t - 1)
  | '\t' | '\r' | '\n' -> t
  | _ -> Helpers.no_trailing_whitespace_in_verbatim (Lexing.lexeme_end lexbuf)
  | exception Invalid_argument _ -> ""



let emit ?start_offset ?adjust_start_by ?adjust_end_by lexbuf token =
  let start =
    match adjust_start_by with
    | None -> Lexing.lexeme_start lexbuf
    | Some s -> Lexing.lexeme_start lexbuf + String.length s
  in
  let start =
    match start_offset with
    | None -> start
    | Some s -> s
  in
  let end_ =
    match adjust_end_by with
    | None -> Lexing.lexeme_end lexbuf
    | Some s -> Lexing.lexeme_end lexbuf - String.length s
  in
  ((start, end_), token)

let reference_token target = function
  | "{!" -> `Simple_reference target
  | "{{!" -> `Begin_reference_with_replacement_text target
  | "{{:" -> `Begin_link_with_replacement_text target
  | _ -> assert false

let heading_level = function
  | '2' -> `Section
  | '3' -> `Subsection
  | '4' -> `Subsubsection
  | _ -> assert false

}



let markup_char =
  ['{' '}' '[' ']' '@']
let space_char =
  [' ' '\t' '\n' '\r']
let bullet_char =
  ['-' '+']
let word_char =
  _ # markup_char # space_char # bullet_char # '\\'

let escape_sequence =
  '\\' markup_char

let horizontal_space =
  [' ' '\t']
let newline =
  '\n' | "\r\n"

let no_brace =
  [^ '}'] # space_char
let no_brace_or_colon =
  [^ ':' '}'] # space_char

let reference_start =
  "{!" | "{{!" | "{{:"

let code_block_text =
  ([^ ']'] | ']'+ [^ ']' '}'])* ']'*
let verbatim_text =
  ([^ 'v'] | 'v'+ [^ 'v' '}'])* 'v'*



rule token = parse
  | horizontal_space* eof
    { emit lexbuf `End }

  | (horizontal_space* newline as prefix)
    horizontal_space* ((newline horizontal_space*)+ as suffix)
    { emit lexbuf `Blank_line ~adjust_start_by:prefix ~adjust_end_by:suffix }

  | horizontal_space* newline horizontal_space*
    { emit lexbuf `Single_newline }

  | horizontal_space+
    { emit lexbuf `Space }

  | (horizontal_space* (newline horizontal_space*)? as p) '}'
    { emit lexbuf `Right_brace ~adjust_start_by:p }

  | word_char+ as w
    { emit lexbuf (`Word w) }

  | '\\' (markup_char as c)
    { emit lexbuf (`Word (String.make 1 c)) }

  | '\\'
    { emit lexbuf (`Word "\\") }

  | '['
    { code_span (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) lexbuf }

  | '-'
    { emit lexbuf `Minus }

  | '+'
    { emit lexbuf `Plus }

  | "{b"
    { emit lexbuf (`Begin_style `Bold) }

  | "{i"
    { emit lexbuf (`Begin_style `Italic) }

  | "{e"
    { emit lexbuf (`Begin_style `Emphasis) }

  | "{^"
    { emit lexbuf (`Begin_style `Superscript) }

  | "{_"
    { emit lexbuf (`Begin_style `Subscript) }

  | (reference_start as start)
    space_char* (no_brace+ as target) space_char* '}'
    { emit lexbuf (reference_token target start) }

  | "{[" (code_block_text as c) "]}"
    { let c = trim_leading_blank_lines c in
      let c = trim_trailing_blank_lines c in
      emit lexbuf (`Code_block c) }

  | "{v" (verbatim_text as t) "v}"
    { let t = trim_leading_space_or_accept_whitespace t lexbuf in
      let t = trim_trailing_space_or_accept_whitespace t lexbuf in
      let t = trim_leading_blank_lines t in
      let t = trim_trailing_blank_lines t in
      emit lexbuf (`Verbatim t) }

  | "{ul"
    { emit lexbuf (`Begin_list `Unordered) }

  | "{ol"
    { emit lexbuf (`Begin_list `Ordered) }

  | "{li"
    { emit lexbuf (`Begin_list_item `Li) }

  | "{-"
    { emit lexbuf (`Begin_list_item `Dash) }

  | '{' (['2'-'4'] as level) ':' (no_brace+ as label)
    { emit lexbuf (`Begin_section_heading (heading_level level, Some label)) }

  | '{' (['2'-'4'] as level)
    { emit lexbuf (`Begin_section_heading (heading_level level, None)) }

  | "@author" horizontal_space+ ([^ '\r' '\n']* as author)
    { emit lexbuf (`Tag (`Author author)) }

  | "@deprecated"
    { emit lexbuf (`Tag `Deprecated) }

  | "@param" horizontal_space+ ((_ # space_char)+ as name)
    { emit lexbuf (`Tag (`Param name)) }

  | "@raise" horizontal_space+ ((_ # space_char)+ as name)
    { emit lexbuf (`Tag (`Raise name)) }

  | "@return"
    { emit lexbuf (`Tag `Return) }

  | "@see" horizontal_space* '<' ([^ '>']* as url) '>'
    { emit lexbuf (`Tag (`See (`Url, url))) }

  | "@see" horizontal_space* '\'' ([^ '>']* as filename) '\''
    { emit lexbuf (`Tag (`See (`File, filename))) }

  | "@see" horizontal_space* '"' ([^ '>']* as name) '"'
    { emit lexbuf (`Tag (`See (`Document, name))) }

  | "@since" horizontal_space+ ([^ '\r' '\n']* as version)
    { emit lexbuf (`Tag (`Since version)) }

  | "@before" horizontal_space+ ((_ # space_char)+ as version)
    { emit lexbuf (`Tag (`Before version)) }

  | "@version" horizontal_space+ ([^ '\r' '\n']* as version)
    { emit lexbuf (`Tag (`Version version)) }

  | "@canonical" horizontal_space+ ([^ '\r' '\n']* as identifier)
    { emit lexbuf (`Tag (`Canonical identifier)) }



  | '{' (['0' '1' '5'-'9'] | ['0'-'9'] ['0'-'9']+ as level)
    { raise_notrace
        (Helpers.Parse_error {
          start_offset = Lexing.lexeme_start lexbuf + 1;
          end_offset = Lexing.lexeme_end lexbuf;
          text = Printf.sprintf "'%s': bad section level (2-4 allowed)" level;
        }) }

  | '{' ['2'-'4'] ':'
    { Helpers.cannot_be_empty
        (Lexing.lexeme_start lexbuf + 2, Lexing.lexeme_end lexbuf)
        ~what:"heading label" }

  | '{' _?
    { raise_notrace
        (Helpers.Parse_error {
          start_offset = Lexing.lexeme_start lexbuf;
          end_offset = Lexing.lexeme_end lexbuf;
          text = Printf.sprintf "'%s': bad markup" (Lexing.lexeme lexbuf);
        }) }

  | ']'
    { raise_notrace
        (Helpers.Parse_error {
          start_offset = Lexing.lexeme_start lexbuf;
          end_offset = Lexing.lexeme_end lexbuf;
          text = "unpaired ']' (end of code)"
        }) }

  | '@' ("author" | "since" | "version" | "canonical")
    { Helpers.cannot_be_empty
        (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
        ~what:(Printf.sprintf "'%s'" (Lexing.lexeme lexbuf)) }

  | '@' ("param" | "raise" | "before")
    { raise_notrace
        (Helpers.Parse_error {
          start_offset = Lexing.lexeme_start lexbuf;
          end_offset = Lexing.lexeme_end lexbuf;
          text =
            Printf.sprintf
              "'%s' expects parameter name on the same line"
              (Lexing.lexeme lexbuf);
        }) }

  | "@see"
    { raise_notrace
        (Helpers.Parse_error {
          start_offset = Lexing.lexeme_start lexbuf;
          end_offset = Lexing.lexeme_end lexbuf;
          text =
            "'@see' must be followed by <url>, 'file', or \"document title\""
        }) }

  | '@' ['a'-'z' 'A'-'Z']+
    { raise_notrace
        (Helpers.Parse_error {
          start_offset = Lexing.lexeme_start lexbuf;
          end_offset = Lexing.lexeme_end lexbuf;
          text = Printf.sprintf "unknown tag '%s'" (Lexing.lexeme lexbuf);
        }) }

  | '@'
    { raise_notrace
        (Helpers.Parse_error {
          start_offset = Lexing.lexeme_start lexbuf;
          end_offset = Lexing.lexeme_end lexbuf;
          text = "stray '@'";
        }) }

  | '\r'
    { raise_notrace
        (Helpers.Parse_error {
          start_offset = Lexing.lexeme_start lexbuf;
          end_offset = Lexing.lexeme_end lexbuf;
          text = "stray '\\r' (carriage return character)";
        }) }

  | reference_start space_char* "}"
    { Helpers.cannot_be_empty
        (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
        ~what:"reference target" }

  | ((reference_start as start) space_char* no_brace+) as prefix
    space_char+ no_brace
    { Helpers.not_allowed
        (Lexing.lexeme_start lexbuf + String.length prefix,
         Lexing.lexeme_end lexbuf - 1)
        ~what:"internal whitespace"
        ~in_what:(Token.describe (reference_token "" start)) }

  | (reference_start as start) [^ '}']* eof
    { Helpers.not_allowed
        (Lexing.lexeme_end lexbuf, Lexing.lexeme_end lexbuf)
        ~what:(Token.describe `End)
        ~in_what:(Token.describe (reference_token "" start)) }

  | "{[" code_block_text eof
    { Helpers.not_allowed
        (Lexing.lexeme_end lexbuf, Lexing.lexeme_end lexbuf)
        ~what:(Token.describe `End)
        ~in_what:(Token.describe (`Code_block "")) }

  | "{v" verbatim_text eof
    { Helpers.not_allowed
        (Lexing.lexeme_end lexbuf, Lexing.lexeme_end lexbuf)
        ~what:(Token.describe `End)
        ~in_what:(Token.describe (`Verbatim "")) }



and code_span buffer nesting_level start_offset = parse
  | ']'
    { if nesting_level = 0 then
        emit lexbuf (`Code_span (Buffer.contents buffer)) ~start_offset
      else begin
        Buffer.add_char buffer ']';
        code_span buffer (nesting_level - 1) start_offset lexbuf
      end }

  | '['
    { Buffer.add_char buffer '[';
      code_span buffer (nesting_level + 1) start_offset lexbuf }

  | "\\]"
    { Buffer.add_char buffer ']';
      code_span buffer nesting_level start_offset lexbuf }

  | newline
    { Helpers.not_allowed
        (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
        ~what:(Token.describe `Single_newline)
        ~in_what:(Token.describe (`Code_span "")) }

  | eof
    { Helpers.not_allowed
        (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
        ~what:(Token.describe `End)
        ~in_what:(Token.describe (`Code_span "")) }

  | _ as c
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset lexbuf }
