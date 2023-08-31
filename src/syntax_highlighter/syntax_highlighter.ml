type infos = (string * (int * int)) list

let tag_of_token (tok : Parser.token) =
  match tok with
  | AMPERAMPER -> "AMPERAMPER"
  | AMPERSAND -> "AMPERSAND"
  | AND -> "AND"
  | AS -> "AS"
  | ASSERT -> "ASSERT"
  | BACKQUOTE -> "BACKQUOTE"
  | BANG -> "BANG"
  | BAR -> "BAR"
  | BARBAR -> "BARBAR"
  | BARRBRACKET -> "BARRBRACKET"
  | BEGIN -> "BEGIN"
  | CHAR _ -> "CHAR"
  | CLASS -> "CLASS"
  | COLON -> "COLON"
  | COLONCOLON -> "COLONCOLON"
  | COLONEQUAL -> "COLONEQUAL"
  | COLONGREATER -> "COLONGREATER"
  | COMMA -> "COMMA"
  | COMMENT _ -> "COMMENT"
  | CONSTRAINT -> "CONSTRAINT"
  | DO -> "DO"
  | DOCSTRING _ -> "DOCSTRING"
  | DONE -> "DONE"
  | DOT -> "DOT"
  | DOTDOT -> "DOTDOT"
  | DOWNTO -> "DOWNTO"
  | ELSE -> "ELSE"
  | END -> "END"
  | EOF -> "EOF"
  | EOL -> "EOL"
  | EQUAL -> "EQUAL"
  | EXCEPTION -> "EXCEPTION"
  | EXTERNAL -> "EXTERNAL"
  | FALSE -> "FALSE"
  | FLOAT _ -> "FLOAT"
  | FOR -> "FOR"
  | FUN -> "FUN"
  | FUNCTION -> "FUNCTION"
  | FUNCTOR -> "FUNCTOR"
  | GREATER -> "GREATER"
  | GREATERRBRACE -> "GREATERRBRACE"
  | GREATERRBRACKET -> "GREATERRBRACKET"
  | IF -> "IF"
  | IN -> "IN"
  | INCLUDE -> "INCLUDE"
  | INFIXOP0 _ -> "INFIXOP0"
  | INFIXOP1 _ -> "INFIXOP1"
  | INFIXOP2 _ -> "INFIXOP2"
  | INFIXOP3 _ -> "INFIXOP3"
  | INFIXOP4 _ -> "INFIXOP4"
  | INHERIT -> "INHERIT"
  | INITIALIZER -> "INITIALIZER"
  | INT _ -> "INT"
  | LABEL _ -> "LABEL"
  | LAZY -> "LAZY"
  | LBRACE -> "LBRACE"
  | LBRACELESS -> "LBRACELESS"
  | LBRACKET -> "LBRACKET"
  | LBRACKETAT -> "LBRACKETAT"
  | LBRACKETATAT -> "LBRACKETATAT"
  | LBRACKETATATAT -> "LBRACKETATATAT"
  | LBRACKETBAR -> "LBRACKETBAR"
  | LBRACKETGREATER -> "LBRACKETGREATER"
  | LBRACKETLESS -> "LBRACKETLESS"
  | LBRACKETPERCENT -> "LBRACKETPERCENT"
  | LBRACKETPERCENTPERCENT -> "LBRACKETPERCENTPERCENT"
  | LESS -> "LESS"
  | LESSMINUS -> "LESSMINUS"
  | LET -> "LET"
  | LIDENT "failwith" -> "failwith"
  | LIDENT _ -> "LIDENT"
  | LPAREN -> "LPAREN"
  | MATCH -> "MATCH"
  | METHOD -> "METHOD"
  | MINUS -> "MINUS"
  | MINUSDOT -> "MINUSDOT"
  | MINUSGREATER -> "MINUSGREATER"
  | MODULE -> "MODULE"
  | MUTABLE -> "MUTABLE"
  | NEW -> "NEW"
  | NONREC -> "NONREC"
  | OBJECT -> "OBJECT"
  | OF -> "OF"
  | OPEN -> "OPEN"
  | OPTLABEL _ -> "OPTLABEL"
  | OR -> "OR"
  | PERCENT -> "PERCENT"
  | PLUS -> "PLUS"
  | PLUSDOT -> "PLUSDOT"
  | PLUSEQ -> "PLUSEQ"
  | PREFIXOP _ -> "PREFIXOP"
  | PRIVATE -> "PRIVATE"
  | QUESTION -> "QUESTION"
  | QUOTE -> "QUOTE"
  | RBRACE -> "RBRACE"
  | RBRACKET -> "RBRACKET"
  | REC -> "REC"
  | RPAREN -> "RPAREN"
  | SEMI -> "SEMI"
  | SEMISEMI -> "SEMISEMI"
  | SIG -> "SIG"
  | STAR -> "STAR"
  | STRING _ -> "STRING"
  | STRUCT -> "STRUCT"
  | THEN -> "THEN"
  | TILDE -> "TILDE"
  | TO -> "TO"
  | TRUE -> "TRUE"
  | TRY -> "TRY"
  | TYPE -> "TYPE"
  | UIDENT _ -> "UIDENT"
  | UNDERSCORE -> "UNDERSCORE"
  | VAL -> "VAL"
  | VIRTUAL -> "VIRTUAL"
  | WHEN -> "WHEN"
  | WHILE -> "WHILE"
  | WITH -> "WITH"
(* Removed *)
#if OCAML_VERSION <= (4,2,3)
  | INT32 _ -> "INT32"
  | INT64 _ -> "INT64"
  | NATIVEINT _ -> "NATIVEINT"
#endif
#if OCAML_VERSION <= (4,3,0)
  | SHARP -> "SHARP"
  | SHARPOP _ -> "SHARPOP"
#endif
(* Added *)
#if OCAML_VERSION >= (4,4,0)
  | HASH -> "HASH"
  | HASHOP _ -> "HASHOP"
#endif
#if OCAML_VERSION >= (4,6,0)
  | DOTOP _ -> "DOTOP"
#endif
#if OCAML_VERSION >= (4,11,0)
  | QUOTED_STRING_EXPR _ -> "QUOTED_STRING_EXPR"
  | QUOTED_STRING_ITEM _ -> "QUOTED_STRING_ITEM"
#endif
#if OCAML_VERSION >= (4,8,0)
  | ANDOP _ -> "ANDOP"
  | LETOP _ -> "LETOP"
#endif

let syntax_highlighting_locs src =
  let lexbuf = Lexing.from_string
#if OCAML_VERSION >= (4,8,0)
      ~with_positions:true
#endif
       src in
  let rec collect lexbuf =
    let tok = Lexer.token_with_comments lexbuf in
    let loc_start, loc_end = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
    let tag = tag_of_token tok in
    match tok with
    | EOF -> []
    | COMMENT (_, loc) ->
        (tag, (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum)) :: collect lexbuf
    | DOCSTRING doc ->
        let loc = Docstrings.docstring_loc doc in
        (tag, (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum)) :: collect lexbuf
    | _ -> (tag, (loc_start.pos_cnum, loc_end.pos_cnum)) :: collect lexbuf
  in
  collect lexbuf
