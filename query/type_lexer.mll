{
  open Type_parser
}

rule token = parse
| ' ' { token lexbuf }
| "-" | "->" { ARROW } (* minus sign is interpreted as an arrow to support partially written queries *)
| "(" { PARENS_OPEN }
| ")" { PARENS_CLOSE }
| "," { COMMA }
| '_' { ANY }
| '*' { STAR }
| "'" (['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']* as p) { POLY p }
| ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_' '.']+ as w { WORD w }
| eof { EOF }
