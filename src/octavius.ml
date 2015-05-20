module Types = OctTypes

module Errors = OctErrors

open OctCommon

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let parse lexbuf =
  let open OctErrors in
    try
      Ok (OctParser.main OctLexer.main lexbuf)
    with
    | ParserError(location, err) ->
        Error {Errors.error = Parser err; location}
    | LexerError(location, err) ->
        Error {Errors.error = Lexer err; location}

let print fmt t =
  OctPrint.pp fmt t
