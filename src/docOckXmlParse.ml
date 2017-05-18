(*
 * Copyright (c) 2014 Leo White <lpw25@cl.cam.ac.uk>
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

type 'a result =
  | Ok of 'a
  | Error of Xmlm.pos option * Xmlm.pos * string

type 'a parser =
  { file : Xmlm.input -> 'a DocOck.Types.Unit.t result;
    unit :  Xmlm.input -> 'a DocOck.Types.Unit.t result;
    text : Xmlm.input -> 'a DocOck.Types.Documentation.text result; }

exception LexerError of Xmlm.pos * Xmlm.pos * string

type 'a token =
  { value: 'a;
    start: Lexing.position;
    finish: Lexing.position; }

let value x = x.value
let start x = x.start
let finish x = x.finish

let position (l, c) =
  Lexing.{ pos_fname = "";
           pos_lnum = l;
           pos_bol = -1;
           pos_cnum = c; }

let build (type base) (input_base : Xmlm.input -> base) =
  let module Parser = DocOckXmlParser.Make(struct type t = base end) in
  let plain_tags = Hashtbl.create 113 in
    Hashtbl.add plain_tags "alias" Parser.ALIAS;
    Hashtbl.add plain_tags "any" Parser.ANY;
    Hashtbl.add plain_tags "apply" Parser.APPLY;
    Hashtbl.add plain_tags "arguments" Parser.ARGUMENTS;
    Hashtbl.add plain_tags "arrow" Parser.ARROW;
    Hashtbl.add plain_tags "author" Parser.AUTHOR;
    Hashtbl.add plain_tags "before" Parser.BEFORE;
    Hashtbl.add plain_tags "bold" Parser.BOLD;
    Hashtbl.add plain_tags "center" Parser.CENTER;
    Hashtbl.add plain_tags "class" Parser.CLASS;
    Hashtbl.add plain_tags "class_type" Parser.CLASS_TYPE;
    Hashtbl.add plain_tags "closed" Parser.CLOSED;
    Hashtbl.add plain_tags "code" Parser.CODE;
    Hashtbl.add plain_tags "column" Parser.COLUMN;
    Hashtbl.add plain_tags "comment" Parser.COMMENT;
    Hashtbl.add plain_tags "constant" Parser.CONSTANT;
    Hashtbl.add plain_tags "constraint" Parser.CONSTRAINT;
    Hashtbl.add plain_tags "constructor" Parser.CONSTRUCTOR;
    Hashtbl.add plain_tags "deprecated" Parser.DEPRECATED;
    Hashtbl.add plain_tags "digest" Parser.DIGEST;
    Hashtbl.add plain_tags "dir" Parser.DIR;
    Hashtbl.add plain_tags "doc" Parser.DOC;
    Hashtbl.add plain_tags "dot" Parser.DOT;
    Hashtbl.add plain_tags "element" Parser.ELEMENT;
    Hashtbl.add plain_tags "emphasize" Parser.EMPHASIZE;
    Hashtbl.add plain_tags "enum" Parser.ENUM;
    Hashtbl.add plain_tags "error" Parser.ERROR;
    Hashtbl.add plain_tags "exception" Parser.EXCEPTION;
    Hashtbl.add plain_tags "expansion" Parser.EXPANSION;
    Hashtbl.add plain_tags "extensible" Parser.EXTENSIBLE;
    Hashtbl.add plain_tags "extension" Parser.EXTENSION;
    Hashtbl.add plain_tags "external" Parser.EXTERNAL;
    Hashtbl.add plain_tags "field" Parser.FIELD;
    Hashtbl.add plain_tags "file" Parser.FILE;
    Hashtbl.add plain_tags "filename" Parser.FILENAME;
    Hashtbl.add plain_tags "fixed" Parser.FIXED;
    Hashtbl.add plain_tags "forward" Parser.FORWARD;
    Hashtbl.add plain_tags "functor" Parser.FUNCTOR;
    Hashtbl.add plain_tags "hidden" Parser.HIDDEN;
    Hashtbl.add plain_tags "identifier" Parser.IDENTIFIER;
    Hashtbl.add plain_tags "import" Parser.IMPORT;
    Hashtbl.add plain_tags "include" Parser.INCLUDE;
    Hashtbl.add plain_tags "index" Parser.INDEX;
    Hashtbl.add plain_tags "inherit" Parser.INHERIT;
    Hashtbl.add plain_tags "inline" Parser.INLINE;
    Hashtbl.add plain_tags "instance_variable" Parser.INSTANCE_VARIABLE;
    Hashtbl.add plain_tags "interface" Parser.INTERFACE;
    Hashtbl.add plain_tags "italic" Parser.ITALIC;
    Hashtbl.add plain_tags "item" Parser.ITEM;
    Hashtbl.add plain_tags "label" Parser.LABEL;
    Hashtbl.add plain_tags "left" Parser.LEFT;
    Hashtbl.add plain_tags "link" Parser.LINK;
    Hashtbl.add plain_tags "line" Parser.LINE;
    Hashtbl.add plain_tags "list" Parser.LIST;
    Hashtbl.add plain_tags "location" Parser.LOCATION;
    Hashtbl.add plain_tags "method" Parser.METHOD;
    Hashtbl.add plain_tags "module" Parser.MODULE;
    Hashtbl.add plain_tags "modules" Parser.MODULES;
    Hashtbl.add plain_tags "module_subst" Parser.MODULE_SUBST;
    Hashtbl.add plain_tags "module_type" Parser.MODULE_TYPE;
    Hashtbl.add plain_tags "mutable" Parser.MUTABLE;
    Hashtbl.add plain_tags "name" Parser.NAME;
    Hashtbl.add plain_tags "neg" Parser.NEG;
    Hashtbl.add plain_tags "newline" Parser.NEWLINE;
    Hashtbl.add plain_tags "object" Parser.OBJECT;
    Hashtbl.add plain_tags "offset" Parser.OFFSET;
    Hashtbl.add plain_tags "open" Parser.OPEN;
    Hashtbl.add plain_tags "optional" Parser.OPTIONAL;
    Hashtbl.add plain_tags "pack" Parser.PACK;
    Hashtbl.add plain_tags "package" Parser.PACKAGE;
    Hashtbl.add plain_tags "param" Parser.PARAM;
    Hashtbl.add plain_tags "path" Parser.PATH;
    Hashtbl.add plain_tags "poly" Parser.POLY;
    Hashtbl.add plain_tags "poly_variant" Parser.POLY_VARIANT;
    Hashtbl.add plain_tags "pos" Parser.POS;
    Hashtbl.add plain_tags "position" Parser.POSITION;
    Hashtbl.add plain_tags "precode" Parser.PRECODE;
    Hashtbl.add plain_tags "primitive" Parser.PRIMITIVE;
    Hashtbl.add plain_tags "private" Parser.PRIVATE;
    Hashtbl.add plain_tags "raise" Parser.RAISE;
    Hashtbl.add plain_tags "record" Parser.RECORD;
    Hashtbl.add plain_tags "reference" Parser.REFERENCE;
    Hashtbl.add plain_tags "resolved" Parser.RESOLVED;
    Hashtbl.add plain_tags "result" Parser.RESULT;
    Hashtbl.add plain_tags "return" Parser.RETURN;
    Hashtbl.add plain_tags "right" Parser.RIGHT;
    Hashtbl.add plain_tags "root" Parser.ROOT;
    Hashtbl.add plain_tags "section" Parser.SECTION;
    Hashtbl.add plain_tags "see" Parser.SEE;
    Hashtbl.add plain_tags "signature" Parser.SIGNATURE;
    Hashtbl.add plain_tags "since" Parser.SINCE;
    Hashtbl.add plain_tags "source" Parser.SOURCE;
    Hashtbl.add plain_tags "special" Parser.SPECIAL;
    Hashtbl.add plain_tags "stop" Parser.STOP;
    Hashtbl.add plain_tags "subscript" Parser.SUBSCRIPT;
    Hashtbl.add plain_tags "subst" Parser.SUBST;
    Hashtbl.add plain_tags "subst_alias" Parser.SUBST_ALIAS;
    Hashtbl.add plain_tags "superscript" Parser.SUPERSCRIPT;
    Hashtbl.add plain_tags "tag" Parser.TAG;
    Hashtbl.add plain_tags "text" Parser.TEXT;
    Hashtbl.add plain_tags "tuple" Parser.TUPLE;
    Hashtbl.add plain_tags "type" Parser.TYPE;
    Hashtbl.add plain_tags "typeof" Parser.TYPEOF;
    Hashtbl.add plain_tags "type_subst" Parser.TYPE_SUBST;
    Hashtbl.add plain_tags "unit" Parser.UNIT;
    Hashtbl.add plain_tags "url" Parser.URL;
    Hashtbl.add plain_tags "value" Parser.VALUE;
    Hashtbl.add plain_tags "var" Parser.VAR;
    Hashtbl.add plain_tags "variant" Parser.VARIANT;
    Hashtbl.add plain_tags "verbatim" Parser.VERBATIM;
    Hashtbl.add plain_tags "version" Parser.VERSION;
    Hashtbl.add plain_tags "virtual" Parser.VIRTUAL;
    Hashtbl.add plain_tags "with" Parser.WITH;
    let lex input () =
      if Xmlm.eoi input then
        let pos = position (Xmlm.pos input) in
        { value = Parser.EOF;
          start = pos;
          finish = pos }
      else
        let start = Xmlm.pos input in
        let token = Xmlm.input input in
        let finish = Xmlm.pos input in
        let value =
          match token with
          | `Dtd _ -> Parser.DTD
          | `Data s -> Parser.Data s
          | `El_start ((namespace, tag), attrs) ->
              if namespace <> DocOckXml.ns then
                raise (LexerError(start, finish,
                                  "unknown namespace " ^ namespace))
              else begin
                try
                  Hashtbl.find plain_tags tag
                with Not_found ->
                  match tag with
                  | "argument" ->
                      let pos =
                        try
                          let pos = List.assoc ("", "pos") attrs in
                            Some (int_of_string pos)
                        with Not_found | Failure _ -> None
                      in
                        Parser.Argument pos
                  | "custom" ->
                      let tag =
                        try
                          List.assoc ("", "tag") attrs
                        with Not_found ->
                          raise (LexerError(start, finish,
                                            "missing tag attribute"))
                      in
                        Parser.Custom tag
                  | "target" ->
                      let name =
                        try
                          Some (List.assoc ("", "name") attrs)
                        with Not_found -> None
                      in
                        Parser.Target name
                  | "title" ->
                      let level =
                        try
                          int_of_string (List.assoc ("", "level") attrs)
                        with
                        | Not_found ->
                            raise (LexerError(start, finish,
                                              "missing level attribute"))
                        | Failure _ ->
                            raise (LexerError(start, finish,
                                              "invalid level attribute"))
                      in
                        Parser.Title level
                  | "base" ->
                      let base = input_base input in
                        if Xmlm.eoi input then
                            raise (LexerError(start, finish,
                                              "unexpected end of file"));
                        let token = Xmlm.input input in
                          if token <> `El_end then
                            raise (LexerError(start, finish,
                                              "unclosed base tag"));
                          Parser.Base base
                  | _ -> raise (LexerError(start, finish,
                                           "unknown tag " ^ tag))
                end
          | `El_end -> Parser.CLOSE
        in
          {value; start = position start; finish = position finish}
    in
    let filep =
      MenhirLib.Convert.traditional2revised value start finish Parser.file
    in
    let unitp =
      MenhirLib.Convert.traditional2revised value start finish Parser.unit
    in
    let textp =
      MenhirLib.Convert.traditional2revised value start finish Parser.text_entry
    in
    let file input =
      try
        Ok (filep (lex input))
      with
      | LexerError(start, finish, msg) -> Error(Some start, finish, msg)
      | Parser.Error ->
          let pos = Xmlm.pos input in
            Error(None, pos, "Syntax error")
      | Xmlm.Error(pos, err) -> Error(None, pos, Xmlm.error_message err)
    in
    let unit input =
      try
        Ok (unitp (lex input))
      with
      | LexerError(start, finish, msg) -> Error(Some start, finish, msg)
      | Parser.Error ->
          let pos = Xmlm.pos input in
            Error(None, pos, "Syntax error")
      | Xmlm.Error(pos, err) -> Error(None, pos, Xmlm.error_message err)
    in
    let text input =
      try
        Ok (textp (lex input))
      with
      | LexerError(start, finish, msg) -> Error(Some start, finish, msg)
      | Parser.Error ->
          let pos = Xmlm.pos input in
            Error(None, pos, "Syntax error")
      | Xmlm.Error(pos, err) -> Error(None, pos, Xmlm.error_message err)
    in
      {file; unit; text}

let text parser = parser.text

let unit parser = parser.unit

let file parser = parser.file
