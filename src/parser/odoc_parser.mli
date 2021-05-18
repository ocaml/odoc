module Ast = Ast
module Location = Location

module Warning : sig
  type t = Warning.t = { location : Location.span; message : string }

  val to_string : t -> string
end

type t = { value : Ast.docs; warnings : Warning.t list }

val parse_comment : location:Lexing.position -> text:string -> t
