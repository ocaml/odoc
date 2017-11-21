{

(* Abort on lexing error. *)

let raise_lexer_error location_span lexer_error =
  raise (Common.LexerError (location_span, lexer_error))



(* Lexer locations. *)

let lexing_position_to_project_position : Lexing.position -> Error.position =
  fun pos -> {
    line = pos.pos_lnum;
    column = pos.pos_cnum - pos.pos_bol;
  }

let current_location_span : Lexing.lexbuf -> Error.location_span =
  fun lexbuf -> {
    start = lexing_position_to_project_position lexbuf.lex_start_p;
    finish = lexing_position_to_project_position lexbuf.lex_curr_p;
  }

let dummy_loc : Error.location_span =
  let dummy_pos = {Error.line = -1; column = -1} in
  {start = dummy_pos; finish = dummy_pos}

let incr_line : Lexing.lexbuf -> unit = fun lexbuf ->
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- {pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }



(* TODO This comment is almost certainly wrong. *)
(* Accumulator for content of preformatted code ({[...]}), verbatim blocks
   ({v ... v}), reference URLs. *)

let preformatted_text_buffer =
  Buffer.create 32

(* TODO This may be redundant with Buffer.length. *)
let buffer_empty =
  ref true

let reset_string_buffer () =
  Buffer.clear preformatted_text_buffer;
  buffer_empty := true

let buffer_char c =
  Buffer.add_char preformatted_text_buffer c;
  buffer_empty := false

let buffer_lexeme lexbuf =
  let s = Lexing.lexeme lexbuf in
  Buffer.add_string preformatted_text_buffer s;
  buffer_empty := false

let get_raw_buffered_string () =
  Buffer.contents preformatted_text_buffer

let remove_opening_blanks s =
  let length = String.length s in
  let rec loop i =
    if i >= length then "" else
    match s.[i] with
    | ' ' | '\009' | '\012' -> loop (i + 1)
    | '\010' ->
        String.sub s (i + 1) (length - (i + 1))
    | '\013' ->
        let j = i + 1 in
          if j < length && s.[j] = '\010' then
            String.sub s (j + 1) (length - (j + 1))
          else
            String.sub s j (length - j)
    | _ -> String.sub s i (length - i)
  in
    loop 0

let remove_closing_blanks s =
  let length = String.length s in
  let rec loop i =
    if i < 0 then "" else
    match s.[i] with
    | ' ' | '\009' | '\012' -> loop (i - 1)
    | '\010' ->
        let j = i - 1 in
          if j >= 0 && s.[j] = '\013' then
            String.sub s 0 j
          else
            String.sub s 0 i
    | _ -> String.sub s 0 (i + 1)
  in
    loop (length - 1)

let get_buffered_string () =
  get_raw_buffered_string ()
  |> remove_opening_blanks
  |> remove_closing_blanks

(* To store the position of the beginning of a
   verbatim string or code section *)
let start_loc = ref Lexing.(dummy_pos, dummy_pos)

let set_start_loc lexbuf =
  start_loc := Lexing.(lexbuf.lex_start_p, lexbuf.lex_curr_p)

let get_start_loc : unit -> Error.location_span = fun () ->
  let start_p, curr_p = !start_loc in
  { start = lexing_position_to_project_position start_p;
    finish = lexing_position_to_project_position curr_p; }

let use_start_loc lexbuf =
  let start_p, _ = !start_loc in
  lexbuf.Lexing.lex_start_p <- start_p

(* To store the positions of nested code sections *)
let inner_start_locs = ref [];;

let push_inner_start_loc lexbuf =
  inner_start_locs := (current_location_span lexbuf) :: !inner_start_locs

let pop_inner_start_loc () =
  match !inner_start_locs with
  | [] -> None
  | l :: rest ->
      inner_start_locs := rest;
      Some l

(* To store the format of a target *)
let target_format = ref None;;

(* To store the kind of a reference *)
let ref_kind = ref Output.RK_element;;

(* To store the start of a see description *)
let see_loc = ref dummy_loc;;

let set_see_loc lexbuf =
  see_loc := current_location_span lexbuf

let get_see_loc () = !see_loc

(* To store the modules of a module list *)
let module_list_modules = ref [];;

let reset_module_list () =
  module_list_modules := [];;

let add_module md =
  module_list_modules := md :: !module_list_modules

let get_module_list () =
  List.rev !module_list_modules

(* Hash table of styles (initialized below) *)
let style_table = Hashtbl.create 19

let () =
  List.iter
    (fun (kwd, tok) -> Hashtbl.add style_table kwd tok)
    [ ("b", Generated_parser.Style Bold);
      ("e", Style Emphasize);
      ("C", Style Center);
      ("L", Style Left);
      ("R", Style Right);
      ("i", Style Italic);
      ("ul", LIST);
      ("ol", ENUM);
      ("li", Item false); ]

(* Hash table of reference kinds (initialized below) *)
let ref_kind_table = Hashtbl.create 19

let () =
  List.iter
    (fun (kind, tok) -> Hashtbl.add ref_kind_table kind tok)
    [ ("val", Output.RK_value);
      ("type", RK_type);
      ("exception", RK_exception);
      ("module", RK_module);
      ("modtype", RK_module_type);
      ("class", RK_class);
      ("classtype", RK_class_type);
      ("attribute", RK_attribute);
      ("method", RK_method);
      ("section", RK_section);
      ("recfield", RK_recfield);
      ("const", RK_const); ]

(* Hash table of tags (initialized below) *)
let tag_table = Hashtbl.create 19

}



let newline = '\010' | "\013\010"
let blank = [' ' '\009' '\012']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let alpha = ['a'-'z' '\223'-'\246' '\248'-'\255' 'A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
let versionchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9'
   '!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '^' '|' '~']

(* The characters which are not the start of any tokens other than Char *)
let safe = [^ ' ' '\009' '\012' '\010' '\013' '\\' '{' '}' '[' ']' '<' 'v' '%' '@' '-' '+']

let escape = '\\' (['{' '}' '[' ']' '@'] as chr)

let ident = alpha identchar* ('.' alpha identchar*)*
let version = versionchar+

(* html marks, to use as alternative markup *)

let html_bold = '<' (['b' 'B'] as tag) blank* '>'
let html_end_bold = "</" ['b' 'B'] blank* '>'
let html_italic = '<' (['i' 'I'] as tag) blank* '>'
let html_end_italic = "</" ['i' 'I'] blank* '>'
let html_title = '<' (['h' 'H'](['0'-'9']+ as num) as tag) blank* '>'
let html_end_title = "</" ['h' 'H'](['0'-'9']+ as num) blank* '>'
let html_list = '<' (['u' 'U']['l' 'L'] as tag) blank* '>'
let html_end_list = "</" ['u' 'U']['l' 'L'] blank* '>'
let html_enum = '<' (['o' 'O']['l' 'L'] as tag) blank* '>'
let html_end_enum = "</" ['o' 'O']['l' 'L'] blank* '>'
let html_item = '<' (['l' 'L']['i' 'I'] as tag) blank* '>'
let html_end_item = "</" ['l' 'L']['i' 'I'] blank* '>'
let html_code = '<' ['c' 'C']['o' 'O']['d' 'D']['e' 'E'] blank* '>'
let html_end_code = "</" ['c' 'C']['o' 'O']['d' 'D']['e' 'E'] blank* '>'
let html_center = '<' (['c' 'C']['e' 'E']['n' 'N']['t' 'T']['e' 'E']['r' 'R'] as tag) blank* '>'
let html_end_center = "</" ['c' 'C']['e' 'E']['n' 'N']['t' 'T']['e' 'E']['r' 'R'] blank* '>'
let html_left = '<' (['l' 'L']['e' 'E']['f' 'F']['t' 'T'] as tag) blank* '>'
let html_end_left = "</" ['l' 'L']['e' 'E']['f' 'F']['t' 'T'] blank* '>'
let html_right = '<' (['r' 'R']['i' 'I']['g' 'G']['h' 'H']['t' 'T'] as tag) blank* '>'
let html_end_right = "</" ['r' 'R']['i' 'I']['g' 'G']['h' 'H']['t' 'T'] blank* '>'



rule main = parse
  | escape
    { Generated_parser.Char (String.make 1 chr) }
  | "@" (ident as tag)
    { try
        let f = Hashtbl.find tag_table tag in
        set_start_loc lexbuf;
        f lexbuf
      with Not_found ->
        Custom tag }
  | "{"
    { BEGIN }
  | "}"
    { END }
  | "{v"
    { reset_string_buffer ();
      set_start_loc lexbuf;
      verb lexbuf }
  | "{%" (identchar+ as fmt) ":"
    { reset_string_buffer ();
      set_start_loc lexbuf;
      target_format := Some fmt;
      target lexbuf }
  | "{%"
    { reset_string_buffer ();
      set_start_loc lexbuf;
      target_format := None;
      target lexbuf }
  | "%}"
    { raise_lexer_error (current_location_span lexbuf) Unmatched_target }
  | "["
    { reset_string_buffer ();
      set_start_loc lexbuf;
      code lexbuf }
  | "]"
    { raise_lexer_error (current_location_span lexbuf) Unmatched_code }
  | "{["
    { reset_string_buffer ();
      set_start_loc lexbuf;
      pre_code lexbuf }
  | "]}"
    { raise_lexer_error (current_location_span lexbuf) Unmatched_pre_code }
  | "{!"
    { reset_string_buffer ();
      set_start_loc lexbuf;
      ref_kind := RK_element;
      reference lexbuf }
  | "{!" (ident as lbl) "}"
    { if lbl = "indexlist" then Special_Ref SRK_index_list
      else Ref(RK_element, lbl) }
  | "{!" (ident as kind) ":"
    { reset_string_buffer ();
      set_start_loc lexbuf;
      if kind = "modules" then begin
        reset_module_list ();
        module_list lexbuf
      end else begin
        let kind =
          try
            Hashtbl.find ref_kind_table kind
          with Not_found -> Output.RK_custom kind
        in
          ref_kind := kind;
          reference lexbuf
      end }
  | "{:"
    { reset_string_buffer ();
      set_start_loc lexbuf;
      ref_kind := RK_link;
      reference lexbuf }
  | "{" (decimal_literal as num) (":" (ident as lbl))?
    { Title (int_of_string num, lbl) }
  | "{" (ident as style)
    { try
        Hashtbl.find style_table style
      with Not_found -> Style (Custom style) }
  | "{-"
    { Item true }
  | "{^"
    { Style Superscript }
  | "{_"
    { Style Subscript }
  | html_code
    { reset_string_buffer ();
      set_start_loc lexbuf;
      html_code lexbuf }
  | html_end_code
    { raise_lexer_error (current_location_span lexbuf) Unmatched_html_code }
  | html_title
    { HTML_Title(tag, int_of_string num) }
  | html_end_title
    { HTML_END_Title (int_of_string num) }
  | html_bold
    { HTML_Bold (String.make 1 tag)}
  | html_end_bold
    { HTML_END_BOLD }
  | html_italic
    { HTML_Italic (String.make 1 tag)}
  | html_end_italic
    { HTML_END_ITALIC }
  | html_center
    { HTML_Center tag}
  | html_end_center
    { HTML_END_CENTER }
  | html_left
    { HTML_Left tag}
  | html_end_left
    { HTML_END_LEFT }
  | html_right
    { HTML_Right tag}
  | html_end_right
    { HTML_END_RIGHT }
  | html_list
    { HTML_List tag}
  | html_end_list
    { HTML_END_LIST }
  | html_enum
    { HTML_Enum tag}
  | html_end_enum
    { HTML_END_ENUM }
  | html_item
    { HTML_Item tag}
  | html_end_item
    { HTML_END_ITEM }
  | "-"
    { MINUS }
  | "+"
    { PLUS }
  | newline
    { incr_line lexbuf;
      NEWLINE }
  | blank+
    { BLANK }
  | safe+
  | _
    { Char (Lexing.lexeme lexbuf) }
  | eof
    { EOF }

and identifier = parse
  | blank+
    { identifier lexbuf }
  | newline
    { incr_line lexbuf;
      identifier lexbuf }
  | ident as id
    { use_start_loc lexbuf;
      id }
  | eof
  | _
    { raise_lexer_error (current_location_span lexbuf) Expected_ident }

and see = parse
  | blank+
    { see lexbuf }
  | newline
    { incr_line lexbuf;
      see lexbuf }
  | "<"
    { reset_string_buffer ();
      set_see_loc lexbuf;
      see_url lexbuf }
  | "'"
    { reset_string_buffer ();
      set_see_loc lexbuf;
      see_file lexbuf }
  | "\""
    { reset_string_buffer ();
      set_see_loc lexbuf;
      see_doc lexbuf }
  | eof
  | _
    { raise_lexer_error (current_location_span lexbuf) Expected_see }

and see_url = parse
  | ">"
    { use_start_loc lexbuf;
      Model.Documentation.Url (get_raw_buffered_string ()) }
  | eof
    { raise_lexer_error (get_see_loc ()) Unterminated_see_url }
  | newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      see_url lexbuf }
  | [^ '>' '\010' '\013' ]+
  | _
    { buffer_lexeme lexbuf;
      see_url lexbuf }

and see_file = parse
  | "'"
    { use_start_loc lexbuf;
      Model.Documentation.File (get_raw_buffered_string ()) }
  | eof
    { raise_lexer_error (get_see_loc ()) Unterminated_see_file }
  | newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      see_file lexbuf }
  | [^ '\'' '\010' '\013' ]+
  | _
    { buffer_lexeme lexbuf;
      see_file lexbuf }

and see_doc = parse
  | "\""
    { use_start_loc lexbuf;
      Model.Documentation.Doc (get_raw_buffered_string ()) }
  | eof
    { raise_lexer_error (get_see_loc ()) Unterminated_see_doc }
  | newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      see_doc lexbuf }
  | [^ '\"' '\010' '\013' ]+
  | _
    { buffer_lexeme lexbuf;
      see_doc lexbuf }

and version = parse
  | blank+
    { version lexbuf }
  | newline
    { incr_line lexbuf;
      version lexbuf }
  | version as v
    { use_start_loc lexbuf;
      v }
  | eof
  | _
    { raise_lexer_error (current_location_span lexbuf) Expected_version }

and verb = parse
  | escape
    { buffer_char chr; verb lexbuf }
  | "{v"
    { raise_lexer_error (current_location_span lexbuf) Nested_verbatim }
  | "v}"
    { use_start_loc lexbuf;
      Verb (get_buffered_string ()) }
  | eof
    { raise_lexer_error (get_start_loc ()) Unterminated_verbatim }
  | newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      verb lexbuf }
  | safe+
  | blank+
  | _
    { buffer_lexeme lexbuf; verb lexbuf }

and target = parse
  | escape
    { buffer_char chr; target lexbuf }
  | "{%"
    { raise_lexer_error (current_location_span lexbuf) Nested_target }
  | "%}"
    { use_start_loc lexbuf;
      Target(!target_format, get_buffered_string ()) }
  | eof
    { raise_lexer_error (get_start_loc ()) Unterminated_target }
  | newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      target lexbuf }
  | safe+
  | blank+
  | _
    { buffer_lexeme lexbuf; target lexbuf }

and code = parse
  | escape
    { buffer_char chr; code lexbuf }
  | "["
    { push_inner_start_loc lexbuf;
      buffer_lexeme lexbuf;
      code lexbuf }
  | "]"
    { match pop_inner_start_loc () with
      | None ->
          use_start_loc lexbuf;
          Code(get_raw_buffered_string ())
      | Some _ ->
          buffer_lexeme lexbuf;
          code lexbuf }
  | eof
    { match pop_inner_start_loc () with
      | None -> raise_lexer_error (get_start_loc ()) Unterminated_code
      | Some l -> raise_lexer_error l Unterminated_code }
  | newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      code lexbuf }
  | safe+
  | blank+
  | _
    { buffer_lexeme lexbuf; code lexbuf }

and pre_code = parse
  | escape
    { buffer_char chr; pre_code lexbuf }
  | "{["
    { raise_lexer_error (current_location_span lexbuf) Nested_pre_code }
  | "]}"
    { use_start_loc lexbuf;
      Pre_Code (get_buffered_string ()) }
  | eof
    { raise_lexer_error (get_start_loc ()) Unterminated_pre_code }
  | newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      pre_code lexbuf }
  | safe+
  | blank+
  | _
    { buffer_lexeme lexbuf; pre_code lexbuf }

and html_code = parse
  | escape
    { buffer_char chr; html_code lexbuf }
  | html_code
    { raise_lexer_error (current_location_span lexbuf) Nested_html_code  }
  | html_end_code
    { use_start_loc lexbuf;
      Code(get_raw_buffered_string ()) }
  | eof
    { raise_lexer_error (get_start_loc ()) Unterminated_html_code }
  | newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      html_code lexbuf }
  | safe+
  | blank+
  | _
    { buffer_lexeme lexbuf; html_code lexbuf }

and reference = parse
  | escape
    { buffer_char chr; reference lexbuf }
  | "}"
    { use_start_loc lexbuf;
      Ref(!ref_kind, get_buffered_string ()) }
  | eof
    { raise_lexer_error (get_start_loc ()) Unterminated_ref }
  | newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      reference lexbuf }
  | safe+
  | blank+
  | _
    { buffer_lexeme lexbuf; reference lexbuf }

and module_list = parse
  | escape
    { buffer_char chr; module_list lexbuf }
  | "}"
    { if not @@ !buffer_empty then add_module (get_buffered_string ());
      use_start_loc lexbuf;
      Special_Ref(SRK_module_list (get_module_list ())) }
  | eof
    { raise_lexer_error (get_start_loc ()) Unterminated_ref }
  | blank+
    { if not @@ !buffer_empty then begin
        add_module (get_buffered_string ());
        reset_string_buffer ()
      end;
      module_list lexbuf }
  | newline
    { incr_line lexbuf;
      if not @@ !buffer_empty then begin
        add_module (get_buffered_string ());
        reset_string_buffer ()
      end;
      module_list lexbuf }
  | safe+
  | _
    { buffer_lexeme lexbuf; module_list lexbuf }

and read_ref = parse
  | "-"
    { Generated_parser.MINUS }
  | "."
    { DOT }
  | [^ '-' '.']+
    { Ref_part (Lexing.lexeme lexbuf) }
  | eof
    { EOF }



{

(* TODO The fact that this calls back into the lexer is seriously broken. *)
(* Initialize tag hash table *)
let () =
  List.iter
    (fun (tag, tok) -> Hashtbl.add tag_table tag tok)
    [ ("author", fun _ -> AUTHOR);
      ("deprecated", fun _ -> DEPRECATED);
      ("param", fun lexbuf -> Param (identifier lexbuf));
      ("raise", fun lexbuf -> Raise (identifier lexbuf));
      ("return", fun _ -> RETURN);
      ("inline", fun _ -> INLINE);
      ("see", fun lexbuf -> See (see lexbuf));
      ("since", fun lexbuf -> Since (version lexbuf));
      ("before", fun lexbuf -> Before (version lexbuf));
      ("version", fun lexbuf -> Version (version lexbuf));
      ("canonical", fun lexbuf -> Canonical (identifier lexbuf)); ]

}
