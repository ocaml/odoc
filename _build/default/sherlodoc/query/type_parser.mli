
(* The type of tokens. *)

type token = 
  | WORD of (string)
  | STAR
  | POLY of (string)
  | PARENS_OPEN
  | PARENS_CLOSE
  | EOF
  | COMMA
  | ARROW
  | ANY

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Db.Typexpr.t)
