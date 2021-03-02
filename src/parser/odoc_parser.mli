module Ast = Ast
module Location = Location

module Error : sig
  type t = Error.t = { location : Location.span; message : string }

  type 'a with_warnings = { value : 'a; warnings : t list }
end

val offset_to_location :
  input:string -> comment_location:Lexing.position -> int -> Location.point

val parse_comment :
  location:Lexing.position -> text:string -> Ast.docs Error.with_warnings
