module Ast = Ast

module Location = Location

module Parse_error = Parse_error

module Token = Token

module Error : sig
  type t = Error.t = { location : Location.span; message : string }

  type 'a with_warnings = { value : 'a; warnings : t list }
end

val offset_to_location :
  input:string -> comment_location:Lexing.position -> int -> Location.point

val parse_comment_raw :
  location:Lexing.position -> text:string -> Ast.docs Error.with_warnings
