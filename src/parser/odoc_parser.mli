module Ast = Ast
module Location_ = Location_

module Error : sig
  type full_location_payload = Error.full_location_payload = {
    location : Location_.span;
    message : string;
  }

  type t = full_location_payload

  type 'a with_warnings = { value : 'a; warnings : t list }

  type warning_accumulator = t list ref
  
  val warning : warning_accumulator -> t -> unit

end

module Parse_error = Parse_error
module Token = Token

val offset_to_location :
  input:string -> comment_location:Lexing.position -> int -> Location_.point

val parse_comment_raw :
  location:Lexing.position -> text:string -> Ast.docs Error.with_warnings
