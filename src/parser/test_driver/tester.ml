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

let open_t =
  [
    ("basic", "@open");
    ("prefix", "@openfoo");
    ("extra_whitespace", "@open");
    ("followed_by_junk", "@open foo");
    ("followed_by_paragraph", "@open\nfoo");
    ("followed_by_tag", "@open\n@deprecated");
    ("with_list", "@open - foo");
  ]

let tags =
  let raise_tests =
    [
      ("raise basic", "@raise Foo");
      ("raise bare", "@raise");
      ("raise words", "@raise foo bar baz");
      ("raise prefix", "@raisefoo");
    ]
  and return_tests =
    [
      ("return basic", "@return");
      ("return words", "@return foo bar");
      ("return prefix", "@returnfoo");
    ]
  and before_tests =
    [
      ("before basic", "@before Foo");
      ("before bare", "@before");
      ("before words", "@before foo bar baz");
      ("before prefix", "@beforefoo");
    ]
  and param_tests =
    [
      ("param basic", "@param foo");
      ("param bare", "@param");
      ("param bare_with_whitespace", "@param");
      ("param immediate_newline", "@param\nfoo");
      ("param followed_by_whitespace", "@param foo");
      ("param extra_whitespace", "@param  foo");
      ("param words", "@param foo bar baz");
      ("param multiline", "@param foo\nbar\nbaz");
      ("param paragraphs", "@param foo bar\n\nbaz");
      ("param two", "@param foo\n@param bar");
      ("param nested", "@param foo @param bar");
      ("param preceded_by_paragraph", "foo\n@param bar");
      ("param prefix", "@paramfoo");
      ("param after_code_block", "{[foo]} @param foo");
    ]
  and deprecated_tests =
    [
      ("basic", "@deprecated");
      ("words", "@deprecated foo bar");
      ("multiline", "@deprecated foo\nbar");
      ("paragraphs", "@deprecated foo\n\nbar");
      ("whitespace_only", "@deprecated");
      ("immediate_newline", "@deprecated\nfoo");
      ("immediate_cr_lf", "@deprecated\r\nfoo");
      ("immediate_blank_line", "@deprecated\n\nfoo");
      ("extra_whitespace", "@deprecated  foo");
      ("followed_by_deprecated", "@deprecated foo\n@deprecated bar");
      ("followed_by_deprecated_cr_lf", "@deprecated foo\r\n@deprecated bar");
      ("nested_in_self", "@deprecated foo @deprecated bar");
      ("nested_in_self_at_start", "@deprecated @deprecated foo");
      ("preceded_by_paragraph", "foo\n@deprecated");
      ("preceded_by_shorthand_list", "- foo\n@deprecated");
      ("with_shorthand_list", "@deprecated - foo");
      ("with_shorthand_list_after_newline", "@deprecated\n- foo");
      ("prefix", "@deprecatedfoo");
      ("after_code_block", "{[foo]} @deprecated");
      ("followed_by_section", "@deprecated foo\n{2 Bar}");
    ]
  in
  raise_tests @ return_tests @ before_tests @ param_tests @ deprecated_tests

let utf8 =
  [
    ("lambda", "\xce\xbb");
    ("words", "\xce\xbb \xce\xbb");
    ("no_validation", "Î");
    ("escapes", "\xce\xbb\\}");
    ("newline", "\xce\xbb \n \xce\xbb");
    ("paragraphs", "\xce\xbb \n\n \xce\xbb");
    ("code_span", "[\xce\xbb]");
    ("minus", "\xce\xbb-\xce\xbb");
    ("shorthand_list", "- \xce\xbb");
    ("styled", "{b \xce\xbb}");
    ("reference_target", "{!\xce\xbb}");
    ("code_block", "{[\xce\xbb]}");
    ("verbatim", "{v \xce\xbb v}");
    ("label", "{2:\xce\xbb Bar}");
    ("author", "@author \xce\xbb");
    ("param", "@param \xce\xbb");
    ("raise", "@raise \xce\xbb");
    ("see", "@see <\xce\xbb>");
    ("since", "@since \xce\xbb");
    ("before", "@before \xce\xbb");
    ("version", "@version \xce\xbb");
    ("right_brace", "\xce\xbb}");
  ]

let bad_markup =
  [
    ("left_brace", "{");
    ("left_brace_with_letter", "{g");
    ("left_brace_with_letters", "{gg");
    ("empty_braces", "{}");
    ("left_space", "{ foo}");
    ("left_spaces", "{  foo}");
    ("left_space_eof", "{");
    ("braces_instead_of_brackets", "{foo}");
    ("right_brace", "}");
    ("right_brace_in_paragraph", "foo}");
    ("multiple_right_brace", "foo } bar } baz");
    ("right_brace_in_list_item", "- foo}");
    ("right_brace_in_code_span", "[foo}]");
    ("right_brace_in_code_block", "{[foo}]}");
    ("right_brace_in_verbatim_text", "{v foo} v}");
    ("right_brace_in_author", "@author Foo}");
    ("right_brace_in_deprecated", "@deprecated }");
    ("right_bracket", "]");
    ("right_bracket_in_paragraph", "foo]");
    ("right_bracket_in_shorthand_list", "- foo]");
    ("right_bracket_in_code_span", "[]]");
    ("right_bracket_in_style", "{b]}");
    ("right_bracket_in_verbatim", "{v ] v}");
    ("right_bracket_in_list", "{ul ]}");
    ("right_bracket_in_list_item", "{ul {li ]}}");
    ("right_bracket_in_heading", "{2 ]}");
    ("right_bracket_in_author", "@author Foo]");
    ("at", "@");
    ("cr", "");
  ]

let dashes =
  [
    ("minus_in_word", "foo-bar");
    ("minus_as_word", "foo -");
    ("plus_in_word", "foo+bar");
    ("plus_as_word", "foo +");
    ("bar_in_word", "foo|bar");
    ("escaped_bar_in_word", "foo\\|bar");
    ("bar_as_word", "foo |");
    ("negative_number", "-3.14 -1337");
    ("n_em_dash", "-- ---");
    ("minus_at", "-@");
    ("at_minus", "-@-");
    ("option", "--option");
  ]

let light_table_tests =
  [
    ("empty_table_light", "{t }");
    ("unclosed_table", "{t ");
    ("simple", {|
        {t
          | a |
        }
      |});
    ("stars", "{t\n  |a|   *b*|\n  |*c| d* |\n}");
    ("backquotes", "{t\n   | `a |`\n}");
    ("no_header", "{t\n |---|---|\n | x | y |\n}");
    ("no_align", "{t\n | x | y |\n | x | y |\n}");
    ("only_align", "{t\n  |--|--|\n}");
    ("no_data", "{t\n | x | y |\n |---|---|\n}");
    ("alignment", "{t\n | a | b | c | d |\n |---|:--|--:|:-:|\n}");
    ("no_bars", "{t\n  a | b | c | d\n ---|:--|--:|:-:\n  a | b | c | d\n}");
    ( "light_table_new_lines",
      "{t\n\n\
      \ | a | b | c | d |\n\n\
      \ |---|---|---|---|\n\n\
      \ | a | b | c | d |\n\n\
       }" );
    ( "light_table_markup",
      "{t\n\
      \ | {i a} {:google.com} \\t | | {m b} {e c} {% xyz %} | {b d} [foo] |\n\
      \ |---|---|---|---|\n\
       }" );
    ( "light_table_markup_with_newlines",
      "{t | h1           | h2          |\n\
      \    |--------------|-------------|\n\
      \    | {e with\n\
      \         newlines} | {b d} [foo] |\n\
       }" );
    ("no_space", "{t\n  | a | b |c| d |\n  |---|--:|:--|:-:|\n}");
    ( "multiple_headers",
      "{t\n||a|b|\n|:-|---:|\n|c|d|\n|cc|dd|\n|-:|:-:|\n|e|f|\n|g|h||\n}" );
    ("block_element_in_cell", "{t\n| {[ a ]} | b |\n|---|---|\n}");
    ("block_element_in_row", "{t\n{[ a ]}\n| a | b |\n|---|---|\n}");
    ("more_cells_later", "{t\n | x | y |\n |---|---|\n | x | y | z |\n}");
    ("less_cells_later", "{t\n | x | y |\n |---|---|\n x \n}");
    ( "multiple_word",
      "{t\n\
       | Header and other word |\n\
       |-----------------------|\n\
       | cell and other words  |\n\
       }" );
    ( "multiple_word_header",
      "{t\n\
       | Header other word |\n\
       |-------------------|\n\
       | Header other word |\n\
       }" );
  ]

let explicit_list_tests =
  [
    ("basic", "{ul {li foo}}");
    ("ordered", "{ol {li foo}}");
    ("two_items", "{ul {li foo} {li bar}}");
    ("items_on_separate_lines", "{ul {li foo}\n{li bar}}");
    ("blank_line", "{ul {li foo}\n\n{li bar}}");
    ("blank_line_in_item", "{ul {li foo\n\nbar}}");
    ("junk", "{ul foo}");
    ("junk_with_no_whitespace", "{ulfoo}");
    ("empty", "{ul}");
    ("unterminated_list", "{ul");
    ("no_whitespace", "{ul{li foo}}");
    ("whitespace_at_end_of_item", "{ul {li foo\n\n\n}}");
    ("unterminated_li_syntax", "{ul {li foo");
    ("unterminated_left_curly_brace", "{ul {- foo");
    ("empty_li_styntax", "{ul {li }}");
    ("empty_left_curly_brace", "{ul {- }}");
    ("li_syntax_without_whitespace", "{ul {lifoo}}");
    ("li_syntax_followed_by_newline", "{ul {li\nfoo}}");
    ("li_syntax_followed_by_cr_lf", "{ul {li\r\nfoo}}");
    ("li_syntax_followed_by_blank_line", "{ul {li\n\nfoo}}");
    ("left_curly_brace_without_whitespace", "{ul {-foo}}");
    ("mixed_list_items", "{ul {li foo} {- bar}}");
    ("nested", "{ul {li {ul {li foo}}}}");
    ("shorthand_in_explicit", "{ul {li - foo\n- bar}}");
    ("explicit_in_shorthand", "- {ul {li foo}}");
    ("bare_li_syntax", "{li foo}");
    ("bare_left_curly_brace", "{- foo");
    ("after_code_block", "{[foo]} {ul {li bar}}");
  ]

let isolated =
  [ ("light list horizontal offset", "- foo bar baz\n   - ba ba ba") ]

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

let all_tests =
  code_cases @ error_recovery @ open_t @ tags @ utf8 @ bad_markup
  @ documentation_cases @ explicit_list_tests

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
         !failure_index !tokens)

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
      | "open" | "o" -> open_t
      | "utf8" | "u" -> utf8
      | "isolated" | "i" -> isolated
      | "tags" | "t" -> tags
      | "light" | "lt" -> light_table_tests
      | "all" -> all_tests
      | "dashes" | "ds" -> dashes
      | "list" | "l" -> explicit_list_tests
      | _ ->
          print_endline "unrecognized argument - running documentation_cases";
          documentation_cases)
    else bad_markup
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
