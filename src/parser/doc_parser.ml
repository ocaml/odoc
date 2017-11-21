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



let parse : Lexing.lexbuf -> (Output.t, Error.t) result = fun lexbuf ->
  let open Error in
  try
    Ok (Generated_parser.main Lexer.main lexbuf)
  with
  | Common.ParserError(location, err) ->
    Error {Error.error = Parser err; location}
  | Common.LexerError(location, err) ->
    Error {Error.error = Lexer err; location}

let parse_ref
    : Lexing.lexbuf -> ((string option * string) list, Error.t) result =
    fun lexbuf ->
  let open Error in
  try
    Ok (Generated_parser.reference_parts Lexer.read_ref lexbuf)
  with
  | Common.ParserError(location, err) ->
    Error {Error.error = Parser err; location}
  | Common.LexerError(location, err) ->
    Error {Error.error = Lexer err; location}

let print : Format.formatter -> Output.t -> unit = fun fmt t ->
  Print.pp fmt t



module Output = Output
module Error = Error
