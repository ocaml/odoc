
rule buffer b = parse
| "&amp;" { Buffer.add_char b '&'; buffer b lexbuf } 
| "&lt;" { Buffer.add_char b '<'; buffer b lexbuf } 
| "&gt;" { Buffer.add_char b '>'; buffer b lexbuf } 
| "&gt" { Buffer.add_char b '>'; buffer b lexbuf } 
| "&gt;" { Buffer.add_char b '>'; buffer b lexbuf } 
| "&quot;" { Buffer.add_char b '>'; buffer b lexbuf } 
| "&#x27;" { Buffer.add_char b '\''; buffer b lexbuf } 
| "&#45" { Buffer.add_char b '-'; buffer b lexbuf } 

| eof { () }
| _ { Buffer.add_string b (Lexing.lexeme lexbuf) ; buffer b lexbuf }

{
let string str =
  let lexbuf = Lexing.from_string str in
  let b = Buffer.create (String.length str) in
  buffer b lexbuf ;
  Buffer.contents b
}