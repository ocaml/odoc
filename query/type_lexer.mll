(* This is the lexer for the [parser.mly]. *)

{
  type token =
  | ARROW
  | PARENS_OPEN
  | PARENS_CLOSE
  | COMMA
  | ANY
  | STAR
  | POLY of string
  | WORD of string
  | EOF
}

rule token = parse
| ' ' { token lexbuf }
(* "-" is treated as "->" because we assume it is an incomplete "->"  *)
| "-" | "->" { ARROW }
| "(" { PARENS_OPEN }
| ")" { PARENS_CLOSE }
| "," { COMMA }
| '_' { ANY }
| '*' { STAR }
| "'" (['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']* as p) { POLY p }
| ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_' '.']+ as w { WORD w }
| eof { EOF }
