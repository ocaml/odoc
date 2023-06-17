(* Internal module, not exposed *)

type output = { ast : Ast.t; warnings : Warning.t list }

val parse : Warning.t list ref -> Token.t Loc.with_location Stream.t -> output
