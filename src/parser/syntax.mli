(* Internal module, not exposed *)

type output = { ast : Ast.t; warnings : Warning.t list }

val parse : Token.t Location.with_location Stream.t -> output
