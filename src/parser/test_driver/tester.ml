open! Odoc_parser_test

module Loc = Odoc_parser.Loc
module Parser = Odoc_parser.Tester

(*
    NOTE (@FayCarsons): for anyone working on this parser - this is probably
    the easiest way to check if something is working. The tests are numerous
    and difficult to parse. A test suite will fail in its entirety if one case
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
    ("Empty style", "{i}");
    ("Empty ref", "{{!www.google.com}}");
    ("Empty link", "{{:www.google.com}}");
    ("List item not at beginning of line", "- foo\n - bar\n- baz");
    ("Empty list item", "{ol {li} }");
    ("'{li' not followed by whitespace", "{ol {lifoo bar baz} }");
    ("End not allowed in table", "{t ");
    ("Empty @author", "@author");
    ("Empty @version", "@version");
  ]

let see = [ ("See", "@see <foo> bar baz quux\n") ]

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
      {|{t\n\
        | Header 1 | Header 2 | Header 3 | Header 4|\n
        | :------: | --------:|:---------|---------|\n
        | centered | right    | left     | default |\n
          omitted  | bar at   | start and| finish\n
        | {e emph} | and | unaligned | bars |\n}|}
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
  area : string;
}

let get_area Loc.{ start; end_; _ } input =
  String.split_on_char '\n' input
  |> List.filteri (fun idx _ -> idx >= pred start.line && idx <= pred end_.line)
  |> List.fold_left ( ^ ) ""

module TokBuf = struct
  type t = {
    tokens : Parser.token Loc.with_location Stream.t;
    mutable idx : int;
    mutable cache : Parser.token Loc.with_location list;
  }

  let cache x self = self.cache <- x :: self.cache

  let create lexbuf =
    let rec fill acc =
      let (Loc.{ value; _ } as loc) = Parser.token lexbuf in
      if Parser.is_EOI value then List.rev @@ (loc :: acc) else fill (loc :: acc)
    in

    let tokens = fill [] in
    { tokens = Stream.of_list tokens; idx = 0; cache = [] }

  let next self =
    let (Loc.{ value = token; _ } as loc) = Stream.next self.tokens in
    self.idx <- succ self.idx;
    cache loc self;
    token

  let failure self label input exn =
    let Loc.{ value; location } = List.hd self.cache in
    { exn; label; offending_token = value; area = get_area location input }
end

let run_test (label, case) =
  let open Either in
  let lexbuf = Lexing.from_string case in
  Lexing.set_filename lexbuf "Tester";
  let tokens = TokBuf.create lexbuf in
  try
    let ast, warnings =
      Parser.run ~filename:"Tester"
      @@ Parser.main (fun _ -> TokBuf.next tokens) lexbuf
    in
    let output = Format.asprintf "%a" parser_output (ast, warnings) in
    Left (label, output)
  with e ->
    let exns = Printexc.to_string e in
    Right (TokBuf.failure tokens label case exns)

let sep = String.init 80 @@ Fun.const '-'

let format_successes =
  let go acc (label, output) =
    Printf.sprintf "%s%s:\n%s\n%s\n" acc label output sep
  in
  List.fold_left go ""

let failure_string { exn; label; offending_token; area } =
  Printf.sprintf {|%s failed with exn: %s
failed on token:
'%s'
input:
"%s"|}
    label exn
    (Parser.string_of_token offending_token)
    area

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
      | "see" | "s" -> see
      | _ ->
          print_endline "unrecognized argument - running documentation_cases";
          documentation_cases)
    else documentation_cases
  in
  let sucesses, failures = List.partition_map run_test cases in
  let sucesses = format_successes sucesses in
  let failures = format_failures @@ List.map failure_string failures in
  Printf.printf
    {| ----SUCCESS---- 
%s

    ----FAILURE----
    %s
    |}
    sucesses failures
