{
type token = ()
}


rule parser buffer =
parse
| "\n---" { Some (Buffer.contents buffer) }
| eof { None}
| _ {Buffer.add_string buffer (Lexing.lexeme lexbuf); parser buffer lexbuf}