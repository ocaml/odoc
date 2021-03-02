type input = {
  file : string;
  offset_to_location : int -> Location.point;
  warnings : Error.warning_accumulator;
  lexbuf : Lexing.lexbuf;
}

val token : input -> Lexing.lexbuf -> Token.t Location.with_location
