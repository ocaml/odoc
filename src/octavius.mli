
module Types = OctTypes

module Errors = OctErrors

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

val parse : Lexing.lexbuf -> (Types.t, Errors.t) result

val print : Format.formatter -> Types.t -> unit
