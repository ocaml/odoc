type input = {
  file : string;
  offset_to_location : int -> Model.Location_.point;
  warnings : Model.Error.warning_accumulator;
  lexbuf : Lexing.lexbuf;
}

val token : input -> Lexing.lexbuf -> Token.t Model.Location_.with_location
