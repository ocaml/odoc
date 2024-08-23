(* Internal module, not exposed *)

type input = {
  file : string;
  offset_to_location : int -> Loc.point;
  mutable warnings : Warning.t list;
}

val token : input -> Lexing.lexbuf -> Parser.token
