module Ast = Ast
module Location_ = Location_
module Error = Error
module Parse_error = Parse_error
module Token = Token

val offset_to_location :
  input:string -> comment_location:Lexing.position -> int -> Location_.point

val parse_comment_raw :
  location:Lexing.position -> text:string -> Ast.docs Error.with_warnings
