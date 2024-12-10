{

open Tokens

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

type math_kind =
  Inline | Block

let math_constr kind x =
  match kind with
  | Inline -> Math_span x
  | Block -> Math_block x

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

let mkloc input lexbuf =
  let open Lexing in
  let pos_to_span pos =  
    Loc.{ line = pos.pos_lnum; 
          column = pos.pos_cnum - pos.pos_bol }
  in
  let file = input.file
  and start = pos_to_span lexbuf.lex_start_p 
  and end_ = pos_to_span lexbuf.lex_curr_p in
  { Loc.file; start; end_ }

let with_loc : Lexing.lexbuf -> input -> 'a -> 'a Loc.with_location = 
  fun lexbuf input  ->
    let location = mkloc input lexbuf in 
    Loc.at location

let with_location_adjustments : (Lexing.lexbuf -> input -> 'a) -> Lexing.lexbuf
  -> input 
  -> ?start_offset:int -> ?adjust_start_by:string -> ?end_offset:int
    -> ?adjust_end_by:string -> 'a
  =
  fun 
    k lexbuf input ?start_offset ?adjust_start_by ?end_offset ?adjust_end_by
    value ->

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
  lexbuf.lex_start_p <- { lexbuf.lex_start_p with pos_cnum = start };
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = end_ };
  k lexbuf input value

let emit = 
  with_location_adjustments (fun _ _ token -> token)


let warning =
  with_location_adjustments (fun lexbuf input error ->
    let location = mkloc input lexbuf in
    input.warnings <- error location :: input.warnings)

let reference_token media start ( target : string ) input lexbuf =
  match start with
  | "{!" -> Simple_ref target
  | "{{!" -> Ref_with_replacement target
  | "{:" -> Simple_link (target)
  | "{{:" -> Link_with_replacement (target)

  | "{image!" -> Media (Reference target, Image)
  | "{image:" -> Media (Link target, Image)
  | "{audio!" -> Media (Reference target, Audio)
  | "{audio:" -> Media (Link target, Audio)
  | "{video!" -> Media (Reference target, Video)
  | "{video:" -> Media (Link target, Video)

  | _ ->
     let target, kind =
       match start with
       | "{{image!" -> Reference target, Image
       | "{{image:" -> Link target, Image
       | "{{audio!" -> Reference target, Audio
       | "{{audio:" -> Link target, Audio
       | "{{video!" -> Reference target, Video
       | "{{video:" -> Link target, Video
       | _ -> assert false
     in
     let token_descr = Tokens.describe (Media_with_replacement (target, kind, "")) in
     let content = media token_descr (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf in
     Media_with_replacement (target, kind, content)

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
  let t = Buffer.contents buffer 
    |> trim_trailing_space_or_accept_whitespace 
    |> trim_leading_space_or_accept_whitespace lexbuf input start_offset 
    |> trim_leading_blank_lines 
    |> trim_trailing_blank_lines 
  in
  emit lexbuf input (Verbatim t) ~start_offset

(* The locations have to be treated carefully in this function. We need to ensure that
   the []`Code_block] location matches the entirety of the block including the terminator,
   and the content location is precicely the location of the text of the code itself.
   Note that the location reflects the content _without_ stripping of whitespace, whereas
   the value of the content in the tree has whitespace stripped from the beginning,
   and trailing empty lines removed. *)
let emit_code_block ~start_offset ~content_offset ~lexbuf input meta delimiter terminator buffer output =
  let content_location = input.offset_to_location content_offset in
  let content = 
    Buffer.contents buffer 
    |> trim_trailing_blank_lines  
    |> with_location_adjustments
        (fun _ _location c ->
          let first_line_offset = content_location.column in
          trim_leading_whitespace ~first_line_offset c)
          lexbuf
          input 
    |> trim_leading_blank_lines 
    |> with_location_adjustments 
        ~adjust_end_by:terminator
        ~start_offset:content_offset 
        with_loc
        lexbuf 
        input 
  in
  emit ~start_offset lexbuf input (Code_block { meta; delimiter; content; output }) 

let heading_level lexbuf input level =
  if String.length level >= 2 && level.[0] = '0' then begin
    warning
      lexbuf input ~start_offset:1 (Parse_error.leading_zero_in_heading_level level)
  end;
  int_of_string level

let buffer_add_lexeme buffer lexbuf =
  Buffer.add_string buffer (Lexing.lexeme lexbuf)

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

let media_start =
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

rule reference_paren_content input start ref_offset start_offset depth_paren
  buffer =
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
    { warning
        lexbuf
        input
        ~start_offset
        (Parse_error.unclosed_bracket ~bracket:"(") ;
      Buffer.contents buffer }
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
    { warning
        lexbuf
        input
        ~start_offset
        (Parse_error.unclosed_bracket ~bracket:start) ;
      Buffer.contents buffer }
  | _
    {
      buffer_add_lexeme buffer lexbuf ;
      reference_content input start start_offset buffer lexbuf }

and token input = parse
  | horizontal_space* eof
    { emit lexbuf input END }

  | ((horizontal_space* newline as prefix)
    horizontal_space* ((newline horizontal_space*)+ as suffix) as ws)
    { 
      Lexing.new_line lexbuf;
      Lexing.new_line lexbuf;
      emit lexbuf input (Blank_line ws) ~adjust_start_by:prefix ~adjust_end_by:suffix }

  | (horizontal_space* newline horizontal_space* as ws)
    {
      Lexing.new_line lexbuf;
      emit lexbuf input (Single_newline ws) }

  | (horizontal_space+ as ws)
    { emit lexbuf input (Space ws) }

  | (horizontal_space* (newline horizontal_space*)? as p) '}'
    { emit lexbuf input RIGHT_BRACE ~adjust_start_by:p }

  | '|'
    { emit lexbuf input BAR }

  | word_char (word_char | bullet_char | '@')*
  | bullet_char (word_char | bullet_char | '@')+ as w
    { emit lexbuf input (Word (unescape_word w)) }

  | '['
    { code_span
        (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }

  | '-'
    { emit lexbuf input MINUS }

  | '+'
    { emit lexbuf input PLUS }

  | "{b"
    { emit lexbuf input (Style `Bold) }

  | "{i"
    { emit lexbuf input (Style `Italic) }

  | "{e"
    { emit lexbuf input (Style `Emphasis) }

  | "{L"
    { emit lexbuf input (Paragraph_style `Left) }

  | "{C"
    { emit lexbuf input (Paragraph_style  `Center) }

  | "{R"
    { emit lexbuf input (Paragraph_style  `Right) }

  | "{^"
    { emit lexbuf input (Style `Superscript) }

  | "{_"
    { emit lexbuf input (Style `Subscript) }

  | "{math" space_char
    { math Block (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }

  | "{m" horizontal_space
    { math Inline (Buffer.create 1024) 0 (Lexing.lexeme_start lexbuf) input lexbuf }


  | "{!modules:"
    { emit lexbuf input MODULES }

| (media_start as start)
    {
      let start_offset = Lexing.lexeme_start lexbuf in
      let target =
        reference_content input start start_offset (Buffer.create 16) lexbuf
      in
      let token = reference_token media start target input lexbuf in
      emit ~start_offset lexbuf input token }

  | "{["
    { code_block (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) None (Buffer.create 256) "" input lexbuf }

  | (("{" (delim_char* as delimiter) "@" horizontal_space*) as prefix) (language_tag_char+ as lang_tag_)
    {
      let start_offset = Lexing.lexeme_start lexbuf in
      let lang_tag =
        with_location_adjustments 
          ~adjust_start_by:prefix 
          with_loc 
          lexbuf 
          input 
          lang_tag_
      in
      let emit_truncated_code_block () =
        let empty_content = with_location_adjustments with_loc lexbuf input "" in
        emit ~start_offset lexbuf input (Code_block { meta = Some { language = lang_tag; tags = None }; delimiter = Some delimiter; content = empty_content; output = None}) 
      in
      match code_block_metadata_tail input lexbuf with
      | Ok metadata -> code_block start_offset (Lexing.lexeme_end lexbuf) (Some metadata) (Buffer.create 256) delimiter input lexbuf
      | Error `Eof ->
          warning lexbuf input ~start_offset Parse_error.truncated_code_block_meta;
          emit_truncated_code_block ()
      | Error (`Invalid_char c) ->
          warning lexbuf input ~start_offset
            (Parse_error.language_tag_invalid_char lang_tag_ c);

          (* NOTE : (@faycarsons) Metadata should not be `None` *)
          code_block start_offset (Lexing.lexeme_end lexbuf) None (Buffer.create 256) delimiter input lexbuf
    }

  | "{@" horizontal_space* '['
    {
      warning lexbuf input Parse_error.no_language_tag_in_meta;
      code_block (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) None (Buffer.create 256) "" input lexbuf
    }

  | "{v"
    { verbatim
        (Buffer.create 1024) None (Lexing.lexeme_start lexbuf) input lexbuf }

  | "{%" ((raw_markup_target as target) ':')? (raw_markup as s)
    ("%}" | eof as e)
    { let token = Raw_markup (target, s) in
      if e <> "%}" then
        warning
          lexbuf
          input
          ~start_offset:(Lexing.lexeme_end lexbuf)
          (Parse_error.not_allowed
            ~what:(Tokens.describe END)
            ~in_what:(Tokens.describe token));
      emit lexbuf input token }

  | "{ul"
    { emit lexbuf input (List `Unordered) }

  | "{ol"
    { emit lexbuf input (List `Ordered) }

  | "{li"
    { emit lexbuf input LI }

  | "{-"
    { emit lexbuf input DASH }

  | "{table"
    { emit lexbuf input TABLE_HEAVY }

  | "{t"
    { emit lexbuf input TABLE_LIGHT }

  | "{tr"
    { emit lexbuf input TABLE_ROW }

  | "{th"
    { emit lexbuf input (Table_cell `Header) }

  | "{td"
    { emit lexbuf input (Table_cell `Data) }

  | '{' (['0'-'9']+ as level) ':' (([^ '}'] # space_char)* as label)
      { emit lexbuf input (Section_heading (heading_level lexbuf input level, Some label)) }

  | '{' (['0'-'9']+ as level)
    { emit lexbuf input (Section_heading (heading_level lexbuf input level, None)) }

  | "@author" ((horizontal_space+ [^ '\r' '\n']*)? as author)
    { emit lexbuf input (Author author) }

  | "@deprecated"
    { emit lexbuf input DEPRECATED }

  | "@param" horizontal_space+ ((_ # space_char)+ as name)
    { emit lexbuf input (Param name) }

  | ("@raise" | "@raises") horizontal_space+ ((_ # space_char)+ as name)
    { emit lexbuf input (Raise name) }

  | ("@return" | "@returns")
    { emit lexbuf input RETURN }

  | "@see" horizontal_space* '<' ([^ '>']* as url) '>'
    { emit lexbuf input (See (`Url, url)) }

  | "@see" horizontal_space* '\'' ([^ '\'']* as filename) '\''
    { emit lexbuf input (See (`File, filename)) }

  | "@see" horizontal_space* '"' ([^ '"']* as name) '"'
    { emit lexbuf input (See (`Document, name)) }

  | "@since" ((horizontal_space+ [^ '\r' '\n']*)? as version)
    { emit lexbuf input (Since version) }

  | "@before" horizontal_space+ ((_ # space_char)+ as version)
    { emit lexbuf input (Before version) }

  | "@version" ((horizontal_space+ [^ '\r' '\n']*)? as version)
    { emit lexbuf input (Version version) }

  | "@canonical" ((horizontal_space+ [^ '\r' '\n']*)? as identifier)
    { emit lexbuf input (Canonical identifier) }

  | "@inline"
    { emit lexbuf input INLINE }

  | "@open"
    { emit lexbuf input OPEN }

  | "@closed"
    { emit lexbuf input CLOSED }

  | "@hidden"
    { emit lexbuf input HIDDEN }

  | "]}"
    { emit lexbuf input RIGHT_CODE_DELIMITER}

  | '{'
    { try bad_markup_recovery (Lexing.lexeme_start lexbuf) input lexbuf
      with Failure _ ->
        warning lexbuf
          input
          (Parse_error.bad_markup
            "{" ~suggestion:"escape the brace with '\\{'.");
        emit lexbuf input (Word "{") }

  | ']'
    { warning lexbuf input Parse_error.unpaired_right_bracket;
      emit lexbuf input (Word "]") }

  | "@param"
    { warning lexbuf input Parse_error.truncated_param;
      emit lexbuf input (Param "") }

  | ("@raise" | "@raises") as tag
    { warning lexbuf input (Parse_error.truncated_raise tag);
      emit lexbuf input (Raise "") }

  | "@before"
    { warning lexbuf input Parse_error.truncated_before;
      emit lexbuf input (Before "") }

  | "@see"
    { warning lexbuf input Parse_error.truncated_see;
      emit lexbuf input (Word "@see") }

  | '@' ['a'-'z' 'A'-'Z']+ as tag
    { warning lexbuf input (Parse_error.unknown_tag tag);
      emit lexbuf input (Word tag) }

  | '@'
    { warning lexbuf input Parse_error.stray_at;
      emit lexbuf input (Word "@") }

  | '\r'
    { warning lexbuf input Parse_error.stray_cr;
      token input lexbuf }

and code_span buffer nesting_level start_offset input = parse
  | ']'
    { if nesting_level = 0 then
        emit lexbuf input (Code_span (Buffer.contents buffer)) ~start_offset
      else begin
        Buffer.add_char buffer ']';
        code_span buffer (nesting_level - 1) start_offset input lexbuf
      end }

  | '['
    { Buffer.add_char buffer '[';
      code_span buffer (nesting_level + 1) start_offset input lexbuf }

  | '\\' ('[' | ']' as c)
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }

  | newline horizontal_space* (newline horizontal_space*)+
    { let w = Parse_error.not_allowed
          ~what:(Tokens.describe (Blank_line "\n\n"))
          ~in_what:(Tokens.describe (Code_span ""))
      in
      warning lexbuf input w;
      Buffer.add_char buffer ' ';
      code_span buffer nesting_level start_offset input lexbuf }
  | newline horizontal_space*
    { Buffer.add_char buffer ' ';
      code_span buffer nesting_level start_offset input lexbuf }

  | eof
    { warning
        lexbuf
        input
        (Parse_error.not_allowed
          ~what:(Tokens.describe END)
          ~in_what:(Tokens.describe (Code_span "")));
      emit lexbuf input (Code_span (Buffer.contents buffer)) ~start_offset }

  | _ as c
    { Buffer.add_char buffer c;
      code_span buffer nesting_level start_offset input lexbuf }

and math kind buffer nesting_level start_offset input = parse
  | '}'
    { if nesting_level == 0 then
        emit lexbuf input (math_constr kind (Buffer.contents buffer)) ~start_offset
      else begin
        Buffer.add_char buffer '}';
        math kind buffer (pred nesting_level) start_offset input lexbuf
      end
      }
  | '{'
    { Buffer.add_char buffer '{';
      math kind buffer (succ nesting_level) start_offset input lexbuf }
  | ("\\{" | "\\}") as s
    { Buffer.add_string buffer s;
      math kind buffer nesting_level start_offset input lexbuf }
  | (newline) as s
    {
      Lexing.new_line lexbuf;
      match kind with
      | Inline ->
        warning 
          lexbuf
          input
          (Parse_error.not_allowed
            ~what:(Tokens.describe (Blank_line "\n"))
            ~in_what:(Tokens.describe (math_constr kind "")));
        Buffer.add_char buffer '\n';
        math kind buffer nesting_level start_offset input lexbuf
      | Block ->
        Buffer.add_string buffer s;
        math kind buffer nesting_level start_offset input lexbuf
    }
  | eof
    { warning
        lexbuf
        input
        (Parse_error.not_allowed
          ~what:(Tokens.describe END)
          ~in_what:(Tokens.describe (math_constr kind "")));
      emit lexbuf input (math_constr kind (Buffer.contents buffer)) ~start_offset }
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
            ~in_what:(Tokens.describe (Verbatim "")))
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
  | [^ '}']+ as text '}' as rest
    { let suggestion =
        Printf.sprintf "did you mean '{!%s}' or '[%s]'?" text text in
      warning
        lexbuf
        input
        ~start_offset
        (Parse_error.bad_markup ("{" ^ rest) ~suggestion);
      emit lexbuf input (Code_span text) ~start_offset}

(* The second field of the metadata.
   This rule keeps whitespaces and newlines in the 'metadata' field except the
   ones just before the '['. *)
and code_block_metadata_tail input = parse
 | (space_char+ as prefix)
   ((space_char* (_ # space_char # ['['])+)+ as meta)
   ((space_char* '[') as suffix)
    {
      let meta =
        with_location_adjustments 
          ~adjust_start_by:prefix 
          ~adjust_end_by:suffix 
          with_loc 
          lexbuf 
          input 
          meta
      in
      Ok (Some meta)
    }
  | (newline | horizontal_space)* '['
    { Ok None }
  | _ as c
    { Error (`Invalid_char c) }
  | eof
    { Error `Eof }

(* NOTE : (@faycarsons) This is currently broken!! *)
and code_block start_offset content_offset metadata prefix delim input = parse
  | ("]" (delim_char* as delim') "[") as terminator
    { if delim = delim' then 
        emit_code_block ~start_offset ~content_offset ~lexbuf input None (Some delim) terminator prefix None
      else
        (Buffer.add_string prefix terminator;
        code_block start_offset content_offset metadata prefix delim input lexbuf) }
  | ("]" (delim_char* as delim') "}") as terminator
    { 
      if delim = delim' then 
        emit_code_block ~start_offset ~content_offset ~lexbuf input None (Some delim ) terminator prefix None
      else (
        Buffer.add_string prefix terminator;
        code_block start_offset content_offset metadata prefix delim input lexbuf
      )
    }
  | eof
    {
      warning lexbuf input ~start_offset Parse_error.truncated_code_block;
      emit_code_block ~start_offset ~content_offset ~lexbuf input None (Some delim ) "" prefix None
    }
  | (_ as c)
    {
      Buffer.add_char prefix c;
      code_block start_offset content_offset metadata prefix delim input lexbuf
    }
