{
open Tokens

(* NOTE: 
   I think that, ideally, this would be rewritten from scratch, removing the 
   currying in `emit` and `with_location_adjustments` that makes this code 
   difficult to understand 

   Additionally, I there think there are probably a few things here that could 
   be better handled by the parser. *)

let unescape_word : string -> string = fun s ->
  (* The common case is that there are no escape sequences. *)
  match String.index s '\\' with
  | exception Not_found -> s
  | _ ->
    let buffer = Buffer.create (String.length s) in
    let rec scan_word index =
      if index >= String.length s then
        ()
      else
        let c = s.[index] in
        let c, increment =
          match c with
          | '\\' ->
            if index + 1 < String.length s then
              match s.[index + 1] with
              | '{' | '}' | '[' | ']' | '@' as c -> c, 2
              | _ -> c, 1
            else c, 1
          | _ -> c, 1
        in
        Buffer.add_char buffer c;
        scan_word (index + increment)
    in
    scan_word 0;
    Buffer.contents buffer


let update_content_newlines : content:string -> Lexing.lexbuf -> unit = 
  fun ~content lexbuf -> 
    String.iter 
      (function '\n' -> Lexing.new_line lexbuf | _ -> ()) 
      content

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

(** Returns [None] for an empty, [Some ident] for an indented line. *)
let trim_leading_whitespace : first_line_offset:int -> string -> string =
 fun ~first_line_offset s ->
  let count_leading_whitespace line =
    let rec count_leading_whitespace' index len =
      if index = len then None
      else
        match line.[index] with
        | ' ' | '\t' -> count_leading_whitespace' (index + 1) len
        | _ -> Some index
    in
    let len = String.length line in
    (* '\r' may remain because we only split on '\n' below. This is important
       for the first line, which would be considered not empty without this check. *)
    let len = if len > 0 && line.[len - 1] = '\r' then len - 1 else len in
    count_leading_whitespace' 0 len
  in

  let lines = Astring.String.cuts ~sep:"\n" s in

  let least_amount_of_whitespace =
    List.fold_left (fun least_so_far line ->
      match (count_leading_whitespace line, least_so_far) with
      | (Some _ as n', None) -> n'
      | (Some n as n', Some least) when n < least -> n'
      | _ -> least_so_far)
  in

  let first_line_max_drop, least_amount_of_whitespace =
    match lines with
    | [] -> 0, None
    | first_line :: tl ->
      begin match count_leading_whitespace first_line with
        | Some n ->
          n, least_amount_of_whitespace (Some (first_line_offset + n)) tl
        | None ->
          0, least_amount_of_whitespace None tl
      end
  in

  match least_amount_of_whitespace with
  | None ->
    s
  | Some least_amount_of_whitespace ->
    let drop n line =
      (* Since blank lines were ignored when calculating
         [least_amount_of_whitespace], their length might be less than the
         amount. *)
      if String.length line < n then line
      else String.sub line n (String.length line - n)
    in
    let lines =
      match lines with
      | [] -> []
      | first_line :: tl ->
        drop (min first_line_max_drop least_amount_of_whitespace) first_line
        :: List.map (drop least_amount_of_whitespace) tl
    in
    String.concat "\n" lines

type input = {
  file : string;
  offset_to_location : int -> Loc.point;
  mutable warnings : Warning.t list;
}


type math_kind =
  Inline | Block

let math_constr input kind inner start_offset =
  let start_pos = input.offset_to_location start_offset in
  match kind with
  | Inline -> Math_span Tokens.{ start = start_pos; inner }
  | Block -> Math_block Tokens.{ start = start_pos; inner }

let with_location_adjustments : 
  (Lexing.lexbuf -> input -> Loc.span -> 'a) 
  -> Lexing.lexbuf
  -> input 
  -> ?start_offset:int 
  -> ?adjust_start_by:string 
  -> ?end_offset:int
  -> ?adjust_end_by:string 
  -> 'a
  = 
  fun k lexbuf input ?start_offset ?adjust_start_by ?end_offset ?adjust_end_by value ->
  let start =
    match start_offset with
    | None -> Lexing.lexeme_start lexbuf
    | Some s -> s
  in
  let start =
    match adjust_start_by with
    | None -> start
    | Some s -> start + String.length s
  in
  let end_ =
    match end_offset with
    | None -> Lexing.lexeme_end lexbuf
    | Some e -> e
  in
  let end_ =
    match adjust_end_by with
    | None -> end_
    | Some s -> end_ - String.length s
  in
  let location = {
    Loc.file = input.file;
    start = input.offset_to_location start;
    end_ = input.offset_to_location end_;
  }
  in
  k lexbuf input location value

let warning =
  with_location_adjustments @@ fun _lexbuf input location error ->
    input.warnings <- error location :: input.warnings

let reference_token lexbuf input media ~opening_delimiter ~start_offset ~inner =
  let start = input.offset_to_location start_offset in
  match opening_delimiter with
  | "{!" -> Simple_ref { inner; start  }
  | "{{!" -> Ref_with_replacement { inner; start }
  | "{:" -> Simple_link { inner; start }
  | "{{:" -> Link_with_replacement { inner; start }

  | "{image!" -> Media { inner = (Reference inner, Image); start }
  | "{image:" -> Media { inner = (Link inner, Image); start }
  | "{audio!" -> Media { inner = (Reference inner, Audio); start }
  | "{audio:" -> Media { inner = (Link inner, Audio); start }
  | "{video!" -> Media { inner = (Reference inner, Video); start }
  | "{video:" -> Media { inner = (Link inner, Video); start }

  | _ ->
     let target, kind =
       match opening_delimiter with
       | "{{image!" -> Reference inner, Image
       | "{{image:" -> Link inner, Image
       | "{{audio!" -> Reference inner, Audio
       | "{{audio:" -> Link inner, Audio
       | "{{video!" -> Reference inner, Video
       | "{{video:" -> Link inner, Video
       | _ -> assert false
     in
     let token_descr = Tokens.describe (Media_with_replacement { inner = (target, kind, ""); start }) in
     let content = media token_descr (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf in
     Media_with_replacement { inner = (target, kind, content); start }

let trim_leading_space_or_accept_whitespace lexbuf input start_offset text =
  match text.[0] with
  | ' ' -> String.sub text 1 (String.length text - 1)
  | '\t' | '\r' | '\n' -> text
  | exception Invalid_argument _ -> ""
  | _ ->
    warning lexbuf
      input
      ~start_offset
      ~end_offset:(start_offset + 2)
      Parse_error.no_leading_whitespace_in_verbatim;
    text

let trim_trailing_space_or_accept_whitespace text =
  match text.[String.length text - 1] with
  | ' ' -> String.sub text 0 (String.length text - 1)
  | '\t' | '\r' | '\n' -> text
  | _ -> text
  | exception Invalid_argument _ -> text

let emit_verbatim lexbuf input start_offset buffer =
  let start = input.offset_to_location start_offset in
  let inner = Buffer.contents buffer 
    |> trim_trailing_space_or_accept_whitespace 
    |> trim_leading_space_or_accept_whitespace lexbuf input start_offset 
    |> trim_leading_blank_lines 
    |> trim_trailing_blank_lines 
  in
  Verbatim { inner; start } 

(* The locations have to be treated carefully in this function. We need to ensure that
   the []`Code_block] location matches the entirety of the block including the terminator,
   and the content location is precicely the location of the text of the code itself.
   Note that the location reflects the content _without_ stripping of whitespace, whereas
   the value of the content in the tree has whitespace stripped from the beginning,
   and trailing empty lines removed. *)
let emit_code_block lexbuf input ~start_offset ~content_offset ~metadata ~delimiter ~terminator ~content has_output =
  let content = Buffer.contents content |> trim_trailing_blank_lines in
  let content_location = input.offset_to_location content_offset in
  let content =
    with_location_adjustments
      (fun _ _location _ c ->
         let first_line_offset = content_location.column in
         trim_leading_whitespace ~first_line_offset c)
      lexbuf
      input 
      content
  in
  let content = trim_leading_blank_lines content in
  let content = 
    with_location_adjustments 
      ~adjust_end_by:terminator 
      ~start_offset:content_offset 
      (fun _ _ -> Loc.at) 
      lexbuf 
      input 
      content 
  in
  let inner = { metadata; delimiter; content } 
  and start = input.offset_to_location start_offset in
  if has_output then 
    Code_block_with_output { inner; start }
  else 
    Code_block { inner; start } 

let heading_level lexbuf input level =
  if String.length level >= 2 && level.[0] = '0' then begin
    let leading_zero = Parse_error.leading_zero_in_heading_level level in
    warning lexbuf input ~start_offset:1 leading_zero
  end;
  int_of_string level

let buffer_add_lexeme buffer lexbuf =
  Buffer.add_string buffer (Lexing.lexeme lexbuf)

let trim_horizontal_start : string -> string = fun s -> 
  let rec go idx = 
    let c = s.[idx] in
      if Char.equal c ' ' 
        then go @@ succ idx 
        else String.sub s idx (String.length s - idx)
  in 
  go 0

}

let markup_char =
  ['{' '}' '[' ']' '@' '|']
let space_char =
  [' ' '\t' '\n' '\r']
let bullet_char =
  ['-' '+']

let word_char =
  (_ # markup_char # space_char # bullet_char) | ('\\' markup_char)

let horizontal_space =
  [' ' '\t']
let newline =
  '\n' | "\r\n"

let reference =
    "{!" | "{{!" | "{:" | "{{:"
  | "{image!" | "{{image!" | "{image:" | "{{image:"
  | "{video!" | "{{video!" | "{video:" | "{{video:"
  | "{audio!" | "{{audio!" | "{audio:" | "{{audio:"

let raw_markup =
  ([^ '%'] | '%'+ [^ '%' '}'])* '%'*

let raw_markup_target =
  ([^ ':' '%'] | '%'+ [^ ':' '%' '}'])* '%'*

let language_tag_char =
  ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' ]

let delim_char =
  ['a'-'z' 'A'-'Z' '0'-'9' '_' ]

rule reference_paren_content input start ref_offset start_offset depth_paren buffer =
  parse
  | '('
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_paren_content input start ref_offset start_offset
        (depth_paren + 1) buffer lexbuf }
  | ')'
    {
      buffer_add_lexeme buffer lexbuf ;
      if depth_paren = 0 then
        reference_content input start ref_offset buffer lexbuf
      else
        reference_paren_content input start ref_offset start_offset
          (depth_paren - 1) buffer lexbuf }
  | eof
    { 
      let unclosed_bracket = 
        Parse_error.unclosed_bracket ~bracket:"("
      in
      warning lexbuf input ~start_offset unclosed_bracket;
      Buffer.contents buffer 
    }
  | _
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_paren_content input start ref_offset start_offset depth_paren
        buffer lexbuf }

and reference_content input start start_offset buffer = parse
  | '}'
    {
      Buffer.contents buffer
    }
  | '('
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_paren_content input start start_offset
        (Lexing.lexeme_start lexbuf) 0 buffer lexbuf
    }
  | '"' [^ '"']* '"'
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_content input start start_offset buffer lexbuf
    }
  | eof
    { 
      let unclosed_bracket = 
        Parse_error.unclosed_bracket ~bracket:start
      in
      warning lexbuf input ~start_offset unclosed_bracket;
      Buffer.contents buffer 
    }
  | _
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_content input start start_offset buffer lexbuf }

and token input = parse
  | horizontal_space* eof
    { END }

  | ((horizontal_space* newline as prefix)
    horizontal_space* ((newline)+ as suffix) as ws)
    {
      (* Account for the first newline we got *)
      update_content_newlines ~content:("\n" ^ prefix ^ suffix) lexbuf;
      Blank_line ws
    }

  | (horizontal_space* newline as ws)
    {
      Lexing.new_line lexbuf;
      Single_newline ws 
    }

  | (horizontal_space+ as ws)
    { Space ws }

  | (horizontal_space* (newline)? as p) '}'
    {
      update_content_newlines ~content:p lexbuf;
      RIGHT_BRACE }

  | '|'
    { BAR }

  | word_char (word_char | bullet_char | '@')*
  | bullet_char (word_char | bullet_char | '@')+ as w
    { (Word (unescape_word w)) }

  | '['
    { code_span
        (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }

  | '-'
    { MINUS }

  | '+'
    { PLUS }

  | "{b"
    { Style Bold  }

  | "{i"
    { Style Italic }

  | "{e"
    { Style Emphasis }

  | "{L"
    { Paragraph_style { inner = Left; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf } }

  | "{C"
    { Paragraph_style { inner = Center; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf } }

  | "{R"
    { Paragraph_style { inner = Right; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf } }

  | "{^"
    { Style Superscript }

  | "{_"
    { Style Subscript }

  | "{math" space_char
    { math Block (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }

  | "{m" horizontal_space
    { math Inline (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }


  | "{!modules:"
    { MODULES }

| (reference as opening_delimiter)
    {
      let start_offset = Lexing.lexeme_start lexbuf in
      let inner =
        reference_content input opening_delimiter start_offset (Buffer.create 16) lexbuf
      in
      reference_token lexbuf input media ~start_offset ~opening_delimiter ~inner 
    }

  | "{["
    { code_block false (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) None (Buffer.create 256) "" input lexbuf }

  | (("{" (delim_char* as delimiter) "@" horizontal_space*) as prefix) (language_tag_char+ as language_tag)
    {
      let start_offset = Lexing.lexeme_start lexbuf in
      let language_tag =
        with_location_adjustments ~adjust_start_by:prefix (fun _ _  -> Loc.at) lexbuf input language_tag
      in
      let emit_truncated_code_block () =
        let empty_content = with_location_adjustments (fun _ _ -> Loc.at) lexbuf input "" in
        Code_block { 
          inner = { 
            metadata = Some { language_tag; tags = None }; 
            delimiter = Some delimiter; 
            content = empty_content 
          }; 
          start = input.offset_to_location start_offset
        }
      in
      (* Disallow result block sections for code blocks without a delimiter.
         This avoids the surprising parsing of '][' ending the code block. *)
      let allow_result_block = delimiter <> "" in
      let code_block_with_metadata metadata =
        let content_offset = Lexing.lexeme_end lexbuf in
        let metadata = Some { language_tag; tags = metadata } in
        let prefix = Buffer.create 256 in
        code_block allow_result_block start_offset content_offset metadata
          prefix delimiter input lexbuf
      in
      match code_block_metadata_tail input lexbuf with
      | Ok metadata -> code_block_with_metadata metadata
      | Error `Eof ->
          warning lexbuf input ~start_offset Parse_error.truncated_code_block_meta;
          emit_truncated_code_block ()
      | Error (`Invalid_char c) ->
          warning lexbuf input ~start_offset
            (Parse_error.language_tag_invalid_char language_tag.Loc.value c);
          code_block_with_metadata None
    }

  | "{@" horizontal_space* '['
    {
      warning lexbuf input Parse_error.no_language_tag_in_meta;
      code_block false (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) None (Buffer.create 256) "" input lexbuf
    }

  | "{v"
    { verbatim (Buffer.create 1024) None (Lexing.lexeme_start lexbuf) input lexbuf }

  | "{%" ((raw_markup_target as target) ':')? (raw_markup as s)
    ("%}" | eof as e)
    {
      let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      let token = Raw_markup { inner = (target, s); start } in
      if e <> "%}" then begin
        let not_allowed = 
          Parse_error.not_allowed
              ~what:(Tokens.describe END)
              ~in_what:(Tokens.describe token)
        in
        let start_offset = Lexing.lexeme_end lexbuf in
        warning lexbuf input ~start_offset not_allowed
      end;
      token 
    }

  | "{ul"
    { List Unordered }

  | "{ol"
    { List Ordered }

  | "{li"
    { LI }

  | "{-"
    { DASH }

  | "{table"
    { TABLE_HEAVY }

  | "{t"
    { TABLE_LIGHT }

  | "{tr"
    { TABLE_ROW }

  | "{th"
    { Table_cell `Header }

  | "{td"
    { Table_cell `Data }

  | '{' (['0'-'9']+ as level) ':' (([^ '}'] # space_char)* as label)
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Section_heading { inner = (heading_level lexbuf input level, Some label); start } }

  | '{' (['0'-'9']+ as level)
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Section_heading { inner = (heading_level lexbuf input level, None); start } }

  | "@author" horizontal_space+ (([^ '\r' '\n']*)? as author)
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Tag (Author { inner = (trim_horizontal_start author); start }) }

  | "@deprecated"
    { Tag_with_content DEPRECATED }

  | "@param" horizontal_space+ ((_ # space_char)+ as inner)
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Tag_with_content (Param { inner; start }) }

  | ("@raise" | "@raises") horizontal_space+ ((_ # space_char)+ as inner)
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Tag_with_content (Raise { inner; start })}

  | ("@return" | "@returns")
    { Tag_with_content RETURN }

  | ("@children_order")
    { Tag_with_content CHILDREN_ORDER }

  | ("@toc_status")
    { Tag_with_content TOC_STATUS }

  | ("@order_category")
    { Tag_with_content ORDER_CATEGORY }

  | ("@short_title")
    { Tag_with_content SHORT_TITLE }

  | "@see" horizontal_space* '<' ([^ '>']* as url) '>'
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Tag_with_content (See { inner = (URL, trim_horizontal_start url); start }) }

  | "@see" horizontal_space* '\'' ([^ '\'']* as filename) '\''
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Tag_with_content (See { inner = (File, trim_horizontal_start filename); start }) }

  | "@see" horizontal_space* '"' ([^ '"']* as name) '"'
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Tag_with_content (See { inner = (Document, trim_horizontal_start name); start }) }

  | "@since" horizontal_space+ (([^ '\r' '\n']+) as inner)
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Tag (Since { inner; start }) }
  | "@since" 
    { Tag (Since { inner = ""; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf }) }

  | "@before" horizontal_space+ ((_ # space_char)+ as inner)
    { let start = input.offset_to_location @@ Lexing.lexeme_start lexbuf in
      Tag_with_content (Before { inner; start }) }

  | "@version" horizontal_space+ (([^ '\r' '\n']+) as inner)
    { Tag (Version { inner; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf }) }
  | "@version" 
    { Tag (Version { inner = ""; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf }) }

  | "@canonical" horizontal_space+ (([^ '\r' '\n']+) as inner)
    { Tag (Canonical { inner; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf }) }
  | "@canonical"
    { Tag (Canonical { inner = ""; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf }) }

  | "@inline"
    { Tag INLINE }

  | "@open"
    { Tag OPEN }

  | "@closed"
    { Tag CLOSED }

  | "@hidden"
    { Tag HIDDEN }

  | "]}"
    { RIGHT_CODE_DELIMITER }

  | '{'
    {
      try bad_markup_recovery (Lexing.lexeme_start lexbuf) input lexbuf
      with Failure _ -> begin
        let bad_markup = 
          Parse_error.bad_markup "{" ~suggestion:"escape the brace with '\\{'." 
        in
        warning lexbuf input bad_markup
      end;
      (Word "{") 
    }

  | ']'
    { warning lexbuf input Parse_error.unpaired_right_bracket;
      Word "]" }

  | "@param"
    { warning lexbuf input Parse_error.truncated_param;
      Tag_with_content (Param { inner = ""; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf }) }

  | ("@raise" | "@raises") as tag
    { warning lexbuf input (Parse_error.truncated_raise tag);
      Tag_with_content (Raise { inner = ""; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf }) }

  | "@before"
    { warning lexbuf input Parse_error.truncated_before;
      Tag_with_content (Before { inner = ""; start = input.offset_to_location @@ Lexing.lexeme_start lexbuf }) }

  | "@see"
    { warning lexbuf input Parse_error.truncated_see;
      Word "@see" }

  | '@' ['a'-'z' 'A'-'Z']+ as tag
    { warning lexbuf input (Parse_error.unknown_tag tag);
      Word tag }

  | '@'
    { warning lexbuf input Parse_error.stray_at;
      Word "@" }

  | '\r'
    { warning lexbuf input Parse_error.stray_cr;
      token input lexbuf }

and code_span buffer nesting_level start_offset input = parse
  | ']'
    { 
      if nesting_level = 0 then
        Code_span { inner = (Buffer.contents buffer); start = input.offset_to_location start_offset }
      else begin
        Buffer.add_char buffer ']';
        code_span buffer (nesting_level - 1) start_offset input lexbuf
      end 
    }

  | '['
    { Buffer.add_char buffer '[';
      code_span buffer (nesting_level + 1) start_offset input lexbuf }

  | '\\' ('[' | ']' as c)
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }

  | newline horizontal_space* ((newline horizontal_space*)+ as ws)
    { 
      let not_allowed = 
        Parse_error.not_allowed
          ~what:(Tokens.describe (Blank_line "\n\n"))
          ~in_what:(Tokens.describe (Code_span {inner = ""; start = Loc.dummy_pos}))
      in
      warning lexbuf input not_allowed;
      update_content_newlines ~content:("\n" ^ ws) lexbuf; 
      Buffer.add_char buffer ' ';
      code_span buffer nesting_level start_offset input lexbuf }
  | newline horizontal_space*
    {
      Lexing.new_line lexbuf;
      Buffer.add_char buffer ' ';
      code_span buffer nesting_level start_offset input lexbuf 
    }

  | eof
    { 
      let not_allowed = 
        Parse_error.not_allowed
          ~what:(Tokens.describe END)
          ~in_what:(Tokens.describe (Code_span {inner = ""; start = Loc.dummy_pos}))
      in
      warning lexbuf input not_allowed;
      Code_span { inner = (Buffer.contents buffer); start = input.offset_to_location start_offset }
    }

  | _ as c
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }

and math kind buffer nesting_level start_offset input = parse
  | '}'
    { 
      if nesting_level == 0 then (
        math_constr input kind (Buffer.contents buffer)) start_offset
      else (
        Buffer.add_char buffer '}';
        math kind buffer (pred nesting_level) start_offset input lexbuf)
    }
  | '{'
    { 
      Buffer.add_char buffer '{';
      math kind buffer (succ nesting_level) start_offset input lexbuf 
    }
  | ("\\{" | "\\}") as s
    { Buffer.add_string buffer s;
      math kind buffer nesting_level start_offset input lexbuf }
  | (newline) as s
    {
      Lexing.new_line lexbuf;
      match kind with
      | Inline ->
        let not_allowed = 
          Parse_error.not_allowed
            ~what:(Tokens.describe (Single_newline "\n"))
            ~in_what:(Tokens.describe (math_constr input kind "" start_offset))
        in
        warning lexbuf input not_allowed;
        Buffer.add_char buffer '\n';
        math kind buffer nesting_level start_offset input lexbuf
      | Block ->
        Buffer.add_string buffer s;
        math kind buffer nesting_level start_offset input lexbuf
    }
  | eof
    { 
      let unexpected_eof = 
        Parse_error.end_not_allowed
          ~in_what:(Tokens.describe (math_constr input kind "" start_offset))
      in
      warning lexbuf input unexpected_eof;
      math_constr input kind (Buffer.contents buffer) start_offset
    }
  | _ as c
    { Buffer.add_char buffer c;
      math kind buffer nesting_level start_offset input lexbuf }

and media tok_descr buffer nesting_level start_offset input = parse
  | '}'
    { if nesting_level == 0 then
        Buffer.contents buffer
      else begin
        Buffer.add_char buffer '}';
        media tok_descr buffer (nesting_level - 1) start_offset input lexbuf
      end
      }
  | '{'
    { Buffer.add_char buffer '{';
      media tok_descr buffer (nesting_level + 1) start_offset input lexbuf }
  | ("\\{" | "\\}") as s
    { Buffer.add_string buffer s;
      media tok_descr buffer nesting_level start_offset input lexbuf }
  | eof
    { warning
        lexbuf
        input
        (Parse_error.not_allowed
          ~what:(Tokens.describe END)
          ~in_what:tok_descr);
      Buffer.contents buffer}
  | (newline)
    { Buffer.add_char buffer ' ';
      media tok_descr buffer nesting_level start_offset input lexbuf }
  | _ as c
    { Buffer.add_char buffer c;
      media tok_descr buffer nesting_level start_offset input lexbuf }

and verbatim buffer last_false_terminator start_offset input = parse
  | (space_char as c) "v}"
    { Buffer.add_char buffer c;
      emit_verbatim lexbuf input start_offset buffer }

  | "v}"
    { Buffer.add_string buffer "v}";
      verbatim
        buffer (Some (Lexing.lexeme_start lexbuf)) start_offset input lexbuf }

  | eof
    { begin match last_false_terminator with
      | None ->
        warning
          lexbuf
          input
          (Parse_error.not_allowed
            ~what:(Tokens.describe END)
            ~in_what:(Tokens.describe (Verbatim {inner = ""; start = Loc.dummy_pos})))
      | Some location ->
        warning
          lexbuf
          input
          ~start_offset:location
          ~end_offset:(location + 2)
          Parse_error.no_trailing_whitespace_in_verbatim
      end;
      emit_verbatim lexbuf input start_offset buffer }

  | _ as c
    { Buffer.add_char buffer c;
      verbatim buffer last_false_terminator start_offset input lexbuf }



and bad_markup_recovery start_offset input = parse
  | [^ '}']+ as inner '}' as rest
    { 
      let suggestion =
        Printf.sprintf "did you mean '{!%s}' or '[%s]'?" inner inner 
      in
      let bad_markup = Parse_error.bad_markup ("{" ^ rest) ~suggestion in
      warning lexbuf input ~start_offset bad_markup;
      Code_span { inner; start = input.offset_to_location start_offset }
    }

(* The second field of the metadata.
   This rule keeps whitespaces and newlines in the 'metadata' field except the
   ones just before the '['. *)
and code_block_metadata_tail input = parse
 | (space_char+ as prefix)
   ((space_char* (_ # space_char # ['['])+)+ as meta)
   ((space_char* '[') as suffix)
    {
      let meta =
        with_location_adjustments ~adjust_start_by:prefix ~adjust_end_by:suffix (fun _ _ -> Loc.at) lexbuf input meta
      in
      Ok (Some meta)
    }
  | (newline | horizontal_space)* '['
    { Ok None }
  | _ as c
    { Error (`Invalid_char c) }
  | eof
    { Error `Eof }

and code_block allow_result_block start_offset content_offset metadata buffer delimiter input = parse
  | ("]" (delim_char* as delim') "[") as terminator
    { if delimiter = delim' && allow_result_block then 
        emit_code_block 
          lexbuf 
          input
          ~start_offset
          ~content_offset 
          ~metadata 
          ~delimiter:(Some delimiter) 
          ~terminator 
          ~content:buffer 
          true
      else (
        Buffer.add_string buffer terminator;
        code_block 
          allow_result_block 
          start_offset 
          content_offset metadata
          buffer 
          delimiter 
          input 
          lexbuf
      )
    }
  | ("]" (delim_char* as delim') "}") as terminator
    { 
      if delimiter = delim' then 
        emit_code_block 
          lexbuf 
          input
          ~start_offset
          ~content_offset 
          ~metadata 
          ~delimiter:(Some delimiter) 
          ~terminator 
          ~content:buffer 
          false
      else (
        Buffer.add_string buffer terminator;
        code_block 
          allow_result_block 
          start_offset 
          content_offset  
          metadata
          buffer 
          delimiter 
          input 
          lexbuf
      )
    }
  | eof
    {
      warning lexbuf input ~start_offset Parse_error.truncated_code_block;
      emit_code_block 
        lexbuf 
        input
        ~start_offset
        ~content_offset 
        ~metadata 
        ~delimiter:(Some delimiter) 
        ~terminator:"" 
        ~content:buffer 
        false
    }
  | (_ as c)
    {
      Buffer.add_char buffer c;
      code_block 
        allow_result_block 
        start_offset 
        content_offset 
        metadata
        buffer 
        delimiter 
        input 
        lexbuf
    }
