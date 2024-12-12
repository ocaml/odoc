open! Odoc_parser_test

module Loc = Odoc_parser.Loc
module Parser = Odoc_parser.Tester

(*
    NOTE (@FayCarsons): for anyone working on this parser - this is probably
    the easiest way to check if something is working. The tests are numerous
    and difficult to read. A test suite will fail in its entirety if one case
    throws an exception.

    If you need to test a specific parser rule or test case, add it here and
    `dune exec tester -- {test cases you want to run}`
*)

let code_cases =
  [
    ("basic", "{[foo]}");
    ("empty", "{[]}");
    ("whitespace only", "{[ ]}");
    ("blank line only", "{[\n  \n]}");
    ("whitespace", "{[foo bar]}");
    ("newline", "{[foo\nbar]}");
    ("carriage return", "{[foo\r\nbar]}");
    ("contains blank line", "{[foo\n\nbar]}");
    ("leading whitespace", "{[ foo]}");
  ]

let error_recovery =
  [
    ("Empty @author", "@author");
    ("Empty @version", "@version");
    ("Code block w/ blank line", "[\n\n]");
    ("Empty style", "{i}");
    ("Empty ref", "{{!www.google.com}}");
    ("Empty link", "{{:www.google.com}}");
    ("List item not at beginning of line", "- foo\n - bar\n- baz");
    ("Empty list item", "{ol {li} }");
    ("'{li' not followed by whitespace", "{ol {lifoo bar baz} }");
    ("End not allowed in table", "{t \n| -- | :--: |\n| a | b");
    ("Empty modules", "{!modules: }");
    ("EOI in modules", "{!modules: Foo Bar");
  ]

let utf =
  [
    ("lambda", "\xce\xbb");
    ("words", "\xce\xbb \xce\xbb");
    ("no_validation", "Î");
    ("escapes", "\xce\xbb\\");
    ("newline", "\xce\xbb \n \xce\xbb");
    ("paragraphs", "\xce\xbb \n\n \xce\xbb");
    ("code_span", "[\xce\xbb]");
    ("minuys", "\xce\xbb-\xce\xbb");
    ("shorthand list", "- \xce\xbb");
    ("styled", "{b \xce\xbb}");
    ("reference_target", "{!\xce\xbb}");
    ("code block ", "{[\xce\xbb]}");
    ("verbatim", "{v \xce\xbb v}");
    ("label", "{2:\xce\xbb Bar}");
    ("author", "@author \xce\xbb");
    ("param", "@param \xce\xbb");
    ("raise", "@raise \xce\xbb");
    ("see", "@see <\xce\xbb>");
    ("since", "@since \xce\xbb");
    ("before", "@before \xce\xbb");
    ("version", "@version \xce\xbb");
    ("right brace", "\xce\xbb}");
  ]

(* Cases (mostly) taken from the 'odoc for library authors' document *)
let documentation_cases =
  [
    ("Light list", "- foo\n- bar");
    ("Heavy list", "{ul {li foo} {li bar} {li baz}}");
    ("Simple ref", "{!Stdlib.Buffer}");
    ("Ref w/ replacement", "{{: https://ocaml.org/ }the OCaml website}");
    ("Modules", "{!modules: Foo Bar Baz}");
    ("Block tag", "@see <foo.ml> bar baz quux\n");
    ("Inline tag", "@author Fay Carsons");
    ("Simple tag", "@open");
    ("Math", "{math \\sum_{i=0}^n x^i%}");
    ("Inline_math", "{m x + 4}");
    ( "Heavy table",
      "{table\n\
      \                    {tr\n\
      \                      {th Header 1}\n\
      \                      {th Header 2}\n\
      \                      {th Header 3}\n\
      \                    }\n\
      \                    {tr\n\
      \                      {td Cell 1}\n\
      \                      {td Cell with {e emphasized content}}\n\
      \                      {td {v a block v} }\n\
      \                    }\n\
      \                  }" );
    ( "Light table",
      {|{t
       | Header 1 | Header 2 | Header 3 | Header 4|
       | :------: | --------:|:---------|---------|
       | centered | right    | left     | default |
       omitted  | bar at   | start and| finish
       | {e emph} | and | unaligned | bars |
      }|}
    );
    ("Styled", "{i italicized}");
    ("Inline code", "[fmap Fun.id None]");
    ("Code block", "{[\n let foo = 0 \n]}");
  ]

open Test.Serialize

let error err = Atom (Odoc_parser.Warning.to_string err)
let parser_output formatter (ast, warnings) =
  let value = Ast_to_sexp.(docs loc_at ast) in
  let warnings = List (List.map error warnings) in
  let output =
    List [ List [ Atom "output"; value ]; List [ Atom "warnings"; warnings ] ]
  in
  Sexplib0.Sexp.pp_hum formatter output;
  Format.pp_print_flush formatter ()

type failure = {
  exn : string;
  label : string;
  offending_token : Parser.token;
  failure_index : int;
  tokens : Parser.token list;
}

let mkfailure label exn last failure_index tokens =
  { offending_token = last; failure_index; tokens; exn; label }

let run_test (label, case) =
  let open Either in
  let reversed_newlines = Parser.reversed_newlines ~input:case in
  let lexbuf = Lexing.from_string case in
  let file = "Tester" in
  Lexing.set_filename lexbuf file;
  let tokens = ref [] in
  let input =
    Parser.Lexer.
      {
        warnings = [];
        offset_to_location =
          Parser.offset_to_location ~reversed_newlines
            ~comment_location:lexbuf.lex_curr_p;
        file;
      }
  in
  let failure_index = ref (-1) in
  let offending_token = ref None in
  let get_tok _ =
    incr failure_index;
    let tok = Parser.Lexer.token input lexbuf in
    tokens := tok :: !tokens;
    offending_token := Some tok;
    tok
  in
  try
    let ast, warnings = Parser.run ~input:case @@ Parser.main get_tok lexbuf in
    let warnings = warnings @ input.warnings in
    let output = Format.asprintf "%a" parser_output (ast, warnings) in
    Left (label, output)
  with e ->
    let exns = Printexc.to_string e in
    (*
    let offending_token = List.hd @@ tokbuf.cache in
    let tokens = List.map Loc.value @@ TokBuf.cache_rest tokbuf in
    *)
    Right
      (mkfailure label exns
         (Option.get !offending_token)
         !failure_index (List.rev !tokens))

let sep = String.init 80 @@ Fun.const '-'

let format_successes =
  let go acc (label, output) =
    Printf.sprintf "%s%s:\n%s\n%s\n" acc label output sep
  in
  List.fold_left go ""

let failure_string { exn; label; offending_token; tokens; failure_index } =
  Printf.sprintf
    {|>>> Case '%s' failed with exn: <<<
%s
Offending token:
%d: '%s'
Tokens:
%s|}
    label exn failure_index
    (Parser.string_of_token offending_token)
    (List.fold_left
       (fun acc t -> acc ^ "\n" ^ Parser.string_of_token t)
       "" tokens)

let format_failures =
  let go acc x = acc ^ "\n" ^ sep ^ "\n" ^ x in
  List.fold_left go ""

let () =
  let cases =
    if Array.length Sys.argv > 1 then (
      match Sys.argv.(1) with
      | "code" -> code_cases
      | "recovery" | "r" -> error_recovery
      | "docs" | "d" -> documentation_cases
      | _ ->
          print_endline "unrecognized argument - running documentation_cases";
          documentation_cases)
    else utf
  in
  let sucesses, failures = List.partition_map run_test cases in
  let sucesses = format_successes sucesses in
  let failures = format_failures @@ List.map failure_string failures in
  Printf.printf
    {|<<<<----SUCCESS---->>>> 
%s

<<<<----FAILURE---->>>>
    %s
    |}
    sucesses failures
