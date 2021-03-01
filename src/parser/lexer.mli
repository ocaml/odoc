type input = {
  file : string;
  offset_to_location : int -> Location_.point;
  warnings : Error.warning_accumulator;
  lexbuf : Lexing.lexbuf;
}

val token : input -> Lexing.lexbuf -> Token.t Location_.with_location
