(*
 * Copyright (c) 2015 Leo White <leo@lpw25.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)



let parse
    : Model.Paths.Identifier.label_parent -> Lexing.lexbuf ->
        (Model.Documentation.body, Error.t) result =
    fun parent_definition lexbuf ->

  Helpers.parent_definition := Some parent_definition;
  match Generated_parser.main Lexer.main lexbuf with
  | parsed_result ->
    Helpers.parent_definition := None;
    Ok parsed_result
  | exception Common.ParserError (location, error) ->
    Helpers.parent_definition := None;
    Error {Error.error = Error.Parser error; location}
  | exception Common.LexerError (location, error) ->
    Helpers.parent_definition := None;
    Error {Error.error = Error.Lexer error; location}
  | exception exn ->
    Helpers.parent_definition := None;
    raise exn

let parse_ref
    : Lexing.lexbuf -> ((string option * string) list, Error.t) result =
    fun lexbuf ->

  try
    Ok (Generated_parser.reference_parts Lexer.read_ref lexbuf)
  with
  | Common.ParserError(location, err) ->
    Error {Error.error = Error.Parser err; location}
  | Common.LexerError(location, err) ->
    Error {Error.error = Error.Lexer err; location}



module Error = Error
