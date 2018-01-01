type input = {
  file : string;
  offset_to_location : int -> Model.Location_.point;
  lexbuf : Lexing.lexbuf;
}

val token : input -> Lexing.lexbuf -> Token.t Model.Location_.with_location
