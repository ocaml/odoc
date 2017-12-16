module Test_helpers :
sig
  val test :
    string ->
    ?comment_location:Model.Error.location ->
    string ->
    (Model.Comment.docs, Model.Error.t) result ->
      unit Alcotest.test_case
  (* Creates an Alcotest test case for the comment parser, given:

     - The test name.
     - An optional location for the comment within the fictional input file.
       This is used to calculate correct error locations. If not given, the test
       case assumes the comment is at line 1, column 0 in the fictional input
       file.
     - The comment text to be parsed.
     - The expected result of parsing. This is always either [Ok ast] with some
       expected parse tree [ast], or an error message with a location
       constructed using the helper [error] (below). *)

  val dummy_page : Model.Paths.Identifier.label_parent
  (* A representation of the fictional file that each comment is in during
     comment parser testing. *)

  val error :
    int -> int -> int -> int -> string list ->
      (Model.Comment.docs, Model.Error.t) result
  (* Constructs the representation of a parse error, given:

     - The expected starting line of the error (1-based).
     - The expected starting column of the error (0-based).
     - The expected ending line of the error.
     - The expected ending column of the error.
     - A list of strings to be concatenated into the expected error message. *)
end =
struct
  let dummy_filename = "test-suite"

  let dummy_page =
    let root : Model.Root.t = {
      package = dummy_filename;
      file = Page dummy_filename;
      digest = ""
    }
    in
    Model.Paths.Identifier.Page (root, dummy_filename)

  let to_lexing_position : Model.Error.location -> Lexing.position = fun loc ->
    {
      pos_fname = dummy_filename;
      pos_lnum = loc.line;
      pos_bol = 0;
      pos_cnum = loc.column
    }

  let comment_testable : Model.Comment.docs Alcotest.testable =
    Alcotest.of_pp Print.comment

  let error_testable : Model.Error.t Alcotest.testable =
    let pp_error formatter error =
      Format.pp_print_string formatter (Model.Error.to_string error) in
    Alcotest.of_pp pp_error

  let result_testable
      : ((Model.Comment.docs, Model.Error.t) result) Alcotest.testable =
    Alcotest.result comment_testable error_testable

  let test
      test_name
       ?(comment_location = {Model.Error.line = 1; column = 0})
       comment_text
       expected_result =

    let test_function () =
      let actual_result =
        Parser_.parse_comment
          ~containing_definition:dummy_page
          ~location:(to_lexing_position comment_location)
          ~text:comment_text
      in

      Alcotest.check
        result_testable "document tree is correct" expected_result actual_result
    in

    (test_name, `Quick, test_function)

  let error start_line start_column end_line end_column message_strings =
    Error
      (`With_location {
        Model.Error.file = "test-suite";
        location = {
          start = {
            line = start_line;
            column = start_column;
          };
          end_ = {
            line = end_line;
            column = end_column;
          };
        };
        error = String.concat "" message_strings;
      })
end
open Test_helpers



let tests = [
  "trivial", [
    test "empty"
      ""
      (Ok []);

    test "space"
      " "
      (Ok []);

    test "two spaces"
      "  "
      (Ok []);

    test "tab"
      "\t"
      (Ok []);

    test "mixed space"
      " \t \t"
      (Ok []);

    test "newline"
      "\n"
      (Ok []);

    test "blank line"
      "\n\n"
      (Ok []);

    test "cr-lf"
      "\r\n"
      (Ok []);
  ];



  "one paragraph", [
    test "word"
      "foo"
      (Ok [`Paragraph [`Word "foo"]]);

    test "two words"
      "foo bar"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "bar"]]);

    test "two spaces"
      "foo  bar"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "bar"]]);

    test "mixed space"
      "foo \t \t bar"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "bar"]]);

    test "two lines"
      "foo\nbar"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "bar"]]);

    test "two lines (cr-lf)"
      "foo\r\nbar"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "bar"]]);

    test "leading space"
      " foo"
      (Ok [`Paragraph [`Word "foo"]]);

    test "trailing space"
      "foo "
      (Ok [`Paragraph [`Word "foo"]]);

    test "trailing space on line"
      "foo \nbar"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "bar"]]);

    test "leading space on line"
      "foo\n bar"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "bar"]]);

    test "trailing tab on line"
      "foo\t\nbar"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "bar"]]);

    test "leading tab on line"
      "foo\n\tbar"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "bar"]]);
  ];



  "two paragraphs", [
    test "basic"
      "foo\n\nbar"
      (Ok [`Paragraph [`Word "foo"]; `Paragraph [`Word "bar"]]);

    test "trailing space"
      "foo \n\nbar"
      (Ok [`Paragraph [`Word "foo"]; `Paragraph [`Word "bar"]]);

    test "leading space"
      "foo\n\n bar"
      (Ok [`Paragraph [`Word "foo"]; `Paragraph [`Word "bar"]]);

    test "cr-lf"
      "foo\r\n\r\nbar"
      (Ok [`Paragraph [`Word "foo"]; `Paragraph [`Word "bar"]]);

    test "mixed cr-lf"
      "foo\n\r\nbar"
      (Ok [`Paragraph [`Word "foo"]; `Paragraph [`Word "bar"]]);
  ];



  "plus, minus words", [
    test "minus in word"
      "foo-bar"
      (Ok [`Paragraph [`Word "foo-bar"]]);

    test "minus as word"
      "foo -"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "-"]]);

    test "plus in word"
      "foo+bar"
      (Ok [`Paragraph [`Word "foo+bar"]]);

    test "plus as word"
      "foo +"
      (Ok [`Paragraph [`Word "foo"; `Space; `Word "+"]]);
  ];



  "escape sequence", [
    test "left brace"
      "\\{"
      (Ok [`Paragraph [`Word "{"]]);

    test "left brace in word"
      "foo\\{bar"
      (Ok [`Paragraph [`Word "foo{bar"]]);

    test "right brace"
      "\\}"
      (Ok [`Paragraph [`Word "}"]]);

    test "right brace in word"
      "foo\\}bar"
      (Ok [`Paragraph [`Word "foo}bar"]]);

    test "left bracket"
      "\\["
      (Ok [`Paragraph [`Word "["]]);

    test "left bracket in word"
      "foo\\[bar"
      (Ok [`Paragraph [`Word "foo[bar"]]);

    test "right bracket"
      "\\]"
      (Ok [`Paragraph [`Word "]"]]);

    test "right bracket in word"
      "foo\\]bar"
      (Ok [`Paragraph [`Word "foo]bar"]]);

    test "at"
      "\\@"
      (Ok [`Paragraph [`Word "@"]]);

    test "at in word"
      "foo\\@bar"
      (Ok [`Paragraph [`Word "foo@bar"]]);

    test "trailing backslash"
      "foo\\"
      (Ok [`Paragraph [`Word "foo\\"]]);

    test "non-escape"
      "foo\\bar"
      (Ok [`Paragraph [`Word "foo\\bar"]]);

    test "backslash not escaped"
      "foo\\\\{bar"
      (Ok [`Paragraph [`Word "foo\\{bar"]]);

    test "single backslash"
      "\\"
      (Ok [`Paragraph [`Word "\\"]]);
  ];



  "code span", [
    test "basic"
      "[foo]"
      (Ok [`Paragraph [`Code_span "foo"]]);

    test "empty"
      "[]"
      (Ok [`Paragraph [`Code_span ""]]);

    test "list"
      "[[]]"
      (Ok [`Paragraph [`Code_span "[]"]]);

    test "no markup"
      "[{b]"
      (Ok [`Paragraph [`Code_span "{b"]]);

    test "few escapes"
      "[\\{]"
      (Ok [`Paragraph [`Code_span "\\{"]]);

    test "escaped right bracket"
      "[\\]]"
      (Ok [`Paragraph [`Code_span "]"]]);

    test "whitespace preserved"
      "[ foo  bar ]"
      (Ok [`Paragraph [`Code_span " foo  bar "]]);

    test "no newlines"
      "[foo\n\nbar]"
      (error 1 4 2 0 ["line break is not allowed in '[...]' (code)"]);

    test "cr-lf preserved"
      "[foo\r\nbar]"
      (error 1 4 2 0 ["line break is not allowed in '[...]' (code)"]);

    test "not merged"
      "[foo][bar]"
      (Ok [`Paragraph [`Code_span "foo"; `Code_span "bar"]]);

    test "explicit space"
      "[foo] [bar]"
      (Ok [`Paragraph [`Code_span "foo"; `Space; `Code_span "bar"]]);

    test "unterminated"
      "[foo"
      (error 1 4 1 4 ["end of text is not allowed in '[...]' (code)"]);
  ];



  "bold", [
    test "basic"
      "{b foo}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"])]]);

    test "extra leading whitespace"
      "{b  \t foo}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"])]]);

    test "leading newline"
      "{b\nfoo}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"])]]);

    test "leading cr-lf"
      "{b\r\nfoo}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"])]]);

    test "leading newline and whitespace"
      "{b\n foo}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"])]]);

    test "no leading whitespace"
      "{bfoo}"
      (error 1 0 1 2 ["'{b' must be followed by space, a tab, or a new line"]);

    test "trailing whitespace"
      "{b foo }"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"])]]);

    test "trailing newline"
      "{b foo\n}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"])]]);

    test "trailing cr-lf"
      "{b foo\r\n}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"])]]);

    test "two words"
      "{b foo bar}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"; `Space; `Word "bar"])]]);

    test "not merged"
      "{b foo}{b bar}"
      (Ok [`Paragraph [
        `Styled (`Bold, [`Word "foo"]); `Styled (`Bold, [`Word "bar"])]]);

    test "nested"
      "{b foo{b bar}}"
      (Ok [`Paragraph [
        `Styled (`Bold, [`Word "foo"; `Styled (`Bold, [`Word "bar"])])]]);

    test "newline"
      "{b foo\nbar}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"; `Space; `Word "bar"])]]);

    test "cr-lf"
      "{b foo\r\nbar}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "foo"; `Space; `Word "bar"])]]);

    test "minus"
      "{b -}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "-"])]]);

    test "minus list item"
      "{b foo\n -bar}"
      (error 2 1 2 2
        [
          "'-' (bulleted list item) is not allowed";
            " in '{b ...}' (boldface text)\n";
          "Suggestion: move '-' so it isn't the first thing on the line";
        ]);

    test "plus list item"
      "{b foo\n +bar}"
      (error 2 1 2 2
        [
          "'+' (numbered list item) is not allowed";
            " in '{b ...}' (boldface text)\n";
          "Suggestion: move '+' so it isn't the first thing on the line";
        ]);

    test "immediate minus list item"
      "{b\n-foo}"
      (error 2 0 2 1
        [
          "'-' (bulleted list item) is not allowed";
            " in '{b ...}' (boldface text)\n";
          "Suggestion: move '-' so it isn't the first thing on the line";
        ]);

    test "immediate plus list item"
      "{b\n+foo}"
      (error 2 0 2 1
        [
          "'+' (numbered list item) is not allowed";
            " in '{b ...}' (boldface text)\n";
          "Suggestion: move '+' so it isn't the first thing on the line";
        ]);

    test "blank line"
      "{b foo\n\nbar}"
      (error 2 0 2 0
        ["blank line is not allowed in '{b ...}' (boldface text)"]);

    test "immediate blank line"
      "{b\n\n"
      (error 2 0 2 0
        ["blank line is not allowed in '{b ...}' (boldface text)"]);

    test "end of comment"
      "{b foo"
      (error 1 6 1 6
        ["end of text is not allowed in '{b ...}' (boldface text)"]);

    test "nested code block"
      "{b {[foo]}"
      (error 1 3 1 10
        ["'{[...]}' (code block) is not allowed in '{b ...}' (boldface text)"]);

    test "degenerate"
      "{b}"
      (error 1 0 1 2 ["'{b ...}' (boldface text) cannot be empty"]);

    test "empty"
      "{b }"
      (error 1 0 1 2 ["'{b ...}' (boldface text) cannot be empty"]);
  ];



  "italic", [
    test "basic"
      "{i foo}"
      (Ok [`Paragraph [`Styled (`Italic, [`Word "foo"])]]);

    test "extra leading whitespace"
      "{i  \t foo}"
      (Ok [`Paragraph [`Styled (`Italic, [`Word "foo"])]]);

    test "leading newline"
      "{i\nfoo}"
      (Ok [`Paragraph [`Styled (`Italic, [`Word "foo"])]]);

    test "leading newline and whitespace"
      "{i\n foo}"
      (Ok [`Paragraph [`Styled (`Italic, [`Word "foo"])]]);
  ];



  "emphasis", [
    test "basic"
      "{e foo}"
      (Ok [`Paragraph [`Styled (`Emphasis, [`Word "foo"])]]);

    test "extra leading whitespace"
      "{e  \t foo}"
      (Ok [`Paragraph [`Styled (`Emphasis, [`Word "foo"])]]);

    test "leading newline"
      "{e\nfoo}"
      (Ok [`Paragraph [`Styled (`Emphasis, [`Word "foo"])]]);

    test "leading newline and whitespace"
      "{e\n foo}"
      (Ok [`Paragraph [`Styled (`Emphasis, [`Word "foo"])]]);
  ];



  "superscript", [
    test "basic"
      "{^ foo}"
      (Ok [`Paragraph [`Styled (`Superscript, [`Word "foo"])]]);

    test "extra leading whitespace"
      "{^  \t foo}"
      (Ok [`Paragraph [`Styled (`Superscript, [`Word "foo"])]]);

    test "leading newline"
      "{^\nfoo}"
      (Ok [`Paragraph [`Styled (`Superscript, [`Word "foo"])]]);

    test "leading cr-lf"
      "{^\r\nfoo}"
      (Ok [`Paragraph [`Styled (`Superscript, [`Word "foo"])]]);

    test "leading newline and whitespace"
      "{^\n foo}"
      (Ok [`Paragraph [`Styled (`Superscript, [`Word "foo"])]]);

    test "no whitespace"
      "{^foo}"
      (Ok [`Paragraph [`Styled (`Superscript, [`Word "foo"])]]);

    test "degenerate"
      "{^}"
      (error 1 0 1 2 ["'{^...}' (superscript) cannot be empty"]);

    test "empty"
      "{^ }"
      (error 1 0 1 2 ["'{^...}' (superscript) cannot be empty"]);
  ];



  "subscript", [
    test "basic"
      "{_ foo}"
      (Ok [`Paragraph [`Styled (`Subscript, [`Word "foo"])]]);

    test "extra leading whitespace"
      "{_  \t foo}"
      (Ok [`Paragraph [`Styled (`Subscript, [`Word "foo"])]]);

    test "leading newline"
      "{_\nfoo}"
      (Ok [`Paragraph [`Styled (`Subscript, [`Word "foo"])]]);

    test "leading newline and whitespace"
      "{_\n foo}"
      (Ok [`Paragraph [`Styled (`Subscript, [`Word "foo"])]]);

    test "no whitespace"
      "{_foo}"
      (Ok [`Paragraph [`Styled (`Subscript, [`Word "foo"])]]);

    test "v"
      "{_uv}"
      (Ok [`Paragraph [`Styled (`Subscript, [`Word "uv"])]]);
  ];



  "simple reference", [
    test "basic"
      "{!foo}"
      (Ok [`Paragraph [`Reference (Root ("foo", TUnknown), [])]]);

    test "leading whitespace"
      "{! foo}"
      (Ok [`Paragraph [`Reference (Root ("foo", TUnknown), [])]]);

    test "trailing whitespace"
      "{!foo }"
      (Ok [`Paragraph [`Reference (Root ("foo", TUnknown), [])]]);

    test "adjacent word (leading)"
      "bar{!foo}"
      (Ok [`Paragraph [`Word "bar"; `Reference (Root ("foo", TUnknown), [])]]);

    test "explicit leading space"
      "bar {!foo}"
      (Ok [`Paragraph
        [`Word "bar"; `Space; `Reference (Root ("foo", TUnknown), [])]]);

    test "adjacent word (trailing)"
      "{!foo}bar"
      (Ok [`Paragraph [`Reference (Root ("foo", TUnknown), []); `Word "bar"]]);

    test "explicit trailing space"
      "{!foo} bar"
      (Ok [`Paragraph
        [`Reference (Root ("foo", TUnknown), []); `Space; `Word "bar"]]);

    test "kind"
      "{!val:foo}"
      (Ok [`Paragraph [`Reference (Root ("foo", TUnknown), [])]]);

    test "empty"
      "{!}"
      (error 1 0 1 3 ["reference target cannot be empty"]);

    test "whitespace only"
      "{! }"
      (error 1 0 1 4 ["reference target cannot be empty"]);

    test "internal whitespace"
      "{!foo bar}"
      (error 1 5 1 6
        ["internal whitespace is not allowed in '{!...}' (cross-reference)"]);

    test "unterminated"
      "{!foo"
      (error 1 5 1 5
        ["end of text is not allowed in '{!...}' (cross-reference)"]);

    test "internal whitespace in kind"
      "{!va l:foo}"
      (error 1 4 1 5
        ["internal whitespace is not allowed in '{!...}' (cross-reference)"]);

    test "internal whitespace in referent"
      "{!val:foo bar}"
      (error 1 9 1 10
        ["internal whitespace is not allowed in '{!...}' (cross-reference)"]);

    test "two colons"
      "{!val:foo:bar}"
      (Ok [`Paragraph [`Reference (Root ("bar", TUnknown), [])]]);

    test "space before colon"
      "{!val :foo}"
      (error 1 5 1 6
        ["internal whitespace is not allowed in '{!...}' (cross-reference)"]);

    test "space after colon"
      "{!val: foo}"
      (error 1 6 1 7
        ["internal whitespace is not allowed in '{!...}' (cross-reference)"]);

    test "unterminated after kind"
      "{!val:foo"
      (error 1 9 1 9
        ["end of text is not allowed in '{!...}' (cross-reference)"]);
  ];



  "reference with text", [
    test "basic"
      "{{!foo} bar}"
      (Ok [`Paragraph [`Reference (Root ("foo", TUnknown), [`Word "bar"])]]);

    test "degenerate"
      "{{!foo}}"
      (error 1 0 1 7 ["'{{!...} ...}' (cross-reference) cannot be empty"]);

    test "empty"
      "{{!foo} }"
      (error 1 0 1 7 ["'{{!...} ...}' (cross-reference) cannot be empty"]);

    test "nested markup"
      "{{!foo} {b bar}}"
      (Ok
        [`Paragraph
          [`Reference
            (Root ("foo", TUnknown), [`Styled (`Bold, [`Word "bar"])])]]);

    test "no separating space"
      "{{!foo}bar}"
      (Ok [`Paragraph [`Reference (Root ("foo", TUnknown), [`Word "bar"])]]);

    test "kind"
      "{{!val:foo} bar}"
      (Ok
        [`Paragraph [`Reference (Root ("foo", TUnknown), [`Word "bar"])]]);

    test "nested reference"
      "{{!foo} {!bar}}"
      (error 1 8 1 14
        [
          "'{!...}' (cross-reference) is not allowed in";
            " '{{!...} ...}' (cross-reference)";
        ]);

    test "empty target"
      "{{!} foo}"
      (error 1 0 1 4 ["reference target cannot be empty"]);

    test "whitespace only in target"
      "{{! } foo}"
      (error 1 0 1 5 ["reference target cannot be empty"]);

    test "internal whitespace"
      "{{!foo bar} baz}"
      (error 1 6 1 7
        [
          "internal whitespace is not allowed in";
            " '{{!...} ...}' (cross-reference)";
        ]);

    test "unterminated"
      "{{!foo"
      (error 1 6 1 6
        ["end of text is not allowed in '{{!...} ...}' (cross-reference)"]);
  ];



  "reference target", [
    (* test "empty kind"
      "{!:foo}"
      (error 1 0 1 3 ["'{!...:' (reference kind) cannot be empty"]); *)

    (* test "with kind but empty"
      "{!val:}"
      (error 1 5 1 7 ["':...}' (reference target) cannot be empty"]);

    test "whitespace kind"
      "{! :foo}"
      (error 1 0 1 4 ["'{!...:' (reference kind) cannot be empty"]);

    test "with kind but whitespace"
      "{!val: }"
      (error 1 5 1 8 ["':...}' (reference target) cannot be empty"]);

    test "internal whitespace in kind"
      "{!va l:foo}"
      (error 1 4 1 5
        ["internal whitespace is not allowed in '{!...}' (cross-reference)"]);

    test "internal whitespace in referent"
      "{!val:foo bar}"
      (error 1 9 1 10
        ["internal whitespace is not allowed in '{!...}' (cross-reference)"]);

    test "two colons"
      "{!val:foo:bar}"
      (Ok [`Paragraph [`Reference (Root ("foo:bar", TUnknown), [])]]);

    test "space before colon"
      "{!val :foo}"
      (error 1 5 1 6
        ["internal whitespace is not allowed in '{!...}' (cross-reference)"]);

    test "space after colon"
      "{!val: foo}"
      (error 1 6 1 7
        ["internal whitespace is not allowed in '{!...}' (cross-reference)"]);

    test "unterminated after kind"
      "{!val:foo"
      (error 1 9 1 9
        ["end of text is not allowed in '{!...}' (cross-reference)"]); *)
  ];



  "link", [
    test "basic"
      "{{:foo} bar}"
      (Ok [`Paragraph [`Link ("foo", [`Word "bar"])]]);

    test "nested markup"
      "{{:foo} {b bar}}"
      (Ok [`Paragraph [`Link ("foo", [`Styled (`Bold, [`Word "bar"])])]]);

    test "no separating space"
      "{{:foo}bar}"
      (Ok [`Paragraph [`Link ("foo", [`Word "bar"])]]);

    test "nested link"
      "{{:foo} {{:bar} baz}}"
      (error 1 8 1 15
        [
          "'{{:...} ...}' (external link) is not allowed in";
            " '{{:...} ...}' (external link)";
        ]);

    test "empty target"
      "{{:} foo}"
      (error 1 0 1 4 ["reference target cannot be empty"]);

    test "whitespace only in target"
      "{{: } foo}"
      (error 1 0 1 5 ["reference target cannot be empty"]);

    test "internal whitespace"
      "{{:foo bar} baz}"
      (error 1 6 1 7
        [
          "internal whitespace is not allowed in";
            " '{{:...} ...}' (external link)";
        ]);

    test "unterminated"
      "{{:foo"
      (error 1 6 1 6
        ["end of text is not allowed in '{{:...} ...}' (external link)"]);
  ];



  "code block", [
    test "basic"
      "{[foo]}"
      (Ok [`Code_block "foo"]);

    test "empty"
      "{[]}"
      (error 1 0 1 4 ["'{[...]}' (code block) cannot be empty"]);

    test "whitespace only"
      "{[ ]}"
      (error 1 0 1 5 ["'{[...]}' (code block) cannot be empty"]);

    test "blank line only"
      "{[\n  \n]}"
      (error 1 0 3 2 ["'{[...]}' (code block) cannot be empty"]);

    test "whitespace"
      "{[foo bar]}"
      (Ok [`Code_block "foo bar"]);

    test "newline"
      "{[foo\nbar]}"
      (Ok [`Code_block "foo\nbar"]);

    test "cr-lf"
      "{[foo\r\nbar]}"
      (Ok [`Code_block "foo\r\nbar"]);

    test "blank line"
      "{[foo\n\nbar]}"
      (Ok [`Code_block "foo\n\nbar"]);

    test "leading whitespace"
      "{[ foo]}"
      (Ok [`Code_block " foo"]);

    test "leading tab"
      "{[\tfoo]}"
      (Ok [`Code_block "\tfoo"]);

    test "leading newline"
      "{[\nfoo]}"
      (Ok [`Code_block "foo"]);

    test "leading cr-lf"
      "{[\r\nfoo]}"
      (Ok [`Code_block "foo"]);

    test "leading newlines"
      "{[\n\nfoo]}"
      (Ok [`Code_block "foo"]);

    test "leading newline with space"
      "{[\n foo]}"
      (Ok [`Code_block " foo"]);

    test "leading newline with trash"
      "{[ \nfoo]}"
      (Ok [`Code_block "foo"]);

    test "nested opener"
      "{[{[]}"
      (Ok [`Code_block "{["]);

    test "nested closer"
      "{[foo]}]}"
      (error 1 7 1 8 ["unpaired ']' (end of code)"]);

    test "nested bracket"
      "{[]]}"
      (Ok [`Code_block "]"]);

    test "two nested brackets"
      "{[]]]}"
      (Ok [`Code_block "]]"]);

    test "nested brackets in text"
      "{[foo]]bar]}"
      (Ok [`Code_block "foo]]bar"]);

    test "trailing whitespace"
      "{[foo ]}"
      (Ok [`Code_block "foo "]);

    test "trailing tab"
      "{[foo\t]}"
      (Ok [`Code_block "foo\t"]);

    test "trailing newline"
      "{[foo\n]}"
      (Ok [`Code_block "foo"]);

    test "trailing cr-lf"
      "{[foo\r\n]}"
      (Ok [`Code_block "foo"]);

    test "trailing newlines"
      "{[foo\n\n]}"
      (Ok [`Code_block "foo"]);

    test "preceded by whitespace"
      " {[foo]}"
      (Ok [`Code_block "foo"]);

    test "followed by whitespace"
      "{[foo]} "
      (Ok [`Code_block "foo"]);

    test "two on one line"
      "{[foo]} {[bar]}"
      (error 1 8 1 15 ["'{[...]}' (code block) must begin on its own line"]);

    test "two"
      "{[foo]}\n{[bar]}"
      (Ok [`Code_block "foo"; `Code_block "bar"]);

    test "two with blank line"
      "{[foo]}\n\n{[bar]}"
      (Ok [`Code_block "foo"; `Code_block "bar"]);

    test "followed by words"
      "{[foo]} bar"
      (error 1 8 1 11 ["paragraph must begin on its own line"]);

    test "preceded by words"
      "foo {[bar]}"
      (error 1 4 1 11 ["'{[...]}' (code block) must begin on its own line"]);

    test "preceded by paragraph"
      "foo\n{[bar]}"
      (Ok [`Paragraph [`Word "foo"]; `Code_block "bar"]);

    test "followed by paragraph"
      "{[foo]}\nbar"
      (Ok [`Code_block "foo"; `Paragraph [`Word "bar"]]);

    test "unterminated"
      "{[foo"
      (error 1 5 1 5 ["end of text is not allowed in '{[...]}' (code block)"]);

    test "unterminated, bracket"
      "{[foo]"
      (error 1 6 1 6 ["end of text is not allowed in '{[...]}' (code block)"]);

    test "trailing cr"
      "{[foo\r]}"
      (Ok [`Code_block "foo\r"]);
  ];



  "verbatim", [
    test "basic"
      "{v foo v}"
      (Ok [`Verbatim "foo"]);

    test "empty"
      "{v v}"
      (error 1 0 1 5 ["'{v ... v}' (verbatim text) cannot be empty"]);

    test "degenerate"
      "{vv}"
      (error 1 0 1 4 ["'{v ... v}' (verbatim text) cannot be empty"]);

    test "whitespace only"
      "{v  v}"
      (error 1 0 1 6 ["'{v ... v}' (verbatim text) cannot be empty"]);

    test "blank line only"
      "{v\n  \nv}"
      (error 1 0 3 2 ["'{v ... v}' (verbatim text) cannot be empty"]);

    test "no leading whitespace"
      "{vfoo v}"
      (error 1 0 1 2 ["'{v' must be followed by whitespace"]);

    test "no trailing whitespace"
      "{v foov}"
      (error 1 6 1 8 ["'v}' must be preceded by whitespace"]);

    test "multiple leading whitespace"
      "{v  foo v}"
      (Ok [`Verbatim " foo"]);

    test "multiple trailing whitespace"
      "{v foo  v}"
      (Ok [`Verbatim "foo "]);

    test "leading tab"
      "{v\tfoo v}"
      (Ok [`Verbatim "\tfoo"]);

    test "leading newline"
      "{v\nfoo v}"
      (Ok [`Verbatim "foo"]);

    test "leading cr-lf"
      "{v\r\nfoo v}"
      (Ok [`Verbatim "foo"]);

    test "trailing tab"
      "{v foo\tv}"
      (Ok [`Verbatim "foo\t"]);

    test "trailing newline"
      "{v foo\nv}"
      (Ok [`Verbatim "foo"]);

    test "trailing cr-lf"
      "{v foo\r\nv}"
      (Ok [`Verbatim "foo"]);

    test "internal whitespace"
      "{v foo bar v}"
      (Ok [`Verbatim "foo bar"]);

    test "newline"
      "{v foo\nbar v}"
      (Ok [`Verbatim "foo\nbar"]);

    test "cr-lf"
      "{v foo\r\nbar v}"
      (Ok [`Verbatim "foo\r\nbar"]);

    test "blank line"
      "{v foo\n\nbar v}"
      (Ok [`Verbatim "foo\n\nbar"]);

    test "leading newlines"
      "{v\n\nfoo v}"
      (Ok [`Verbatim "foo"]);

    test "leading newline with space"
      "{v\n foo v}"
      (Ok [`Verbatim " foo"]);

    test "leading newline with trash"
      "{v \nfoo v}"
      (Ok [`Verbatim "foo"]);

    test "nested opener"
      "{v {v v}"
      (Ok [`Verbatim "{v"]);

    test "nested closer"
      "{v foo v} v}"
      (error 1 10 1 11 ["paragraph must begin on its own line"]);

    test "nested v"
      "{v v v}"
      (Ok [`Verbatim "v"]);

    test "two nested vs"
      "{v vv v}"
      (Ok [`Verbatim "vv"]);

    test "nested v at end"
      "{v vv}"
      (error 1 4 1 6 ["'v}' must be preceded by whitespace"]);

    test "two nested vs at end"
      "{v vvv}"
      (error 1 5 1 7 ["'v}' must be preceded by whitespace"]);

    test "nested vs in text"
      "{v foovvbar v}"
      (Ok [`Verbatim "foovvbar"]);

    test "trailing newlines"
      "{v foo\n\nv}"
      (Ok [`Verbatim "foo"]);

    test "preceded by whitespace"
      " {v foo v}"
      (Ok [`Verbatim "foo"]);

    test "followed by whitespace"
      "{v foo v} "
      (Ok [`Verbatim "foo"]);

    test "two on one line"
      "{v foo v} {v bar v}"
      (error 1 10 1 19
        ["'{v ... v}' (verbatim text) must begin on its own line"]);

    test "two"
      "{v foo v}\n{v bar v}"
      (Ok [`Verbatim "foo"; `Verbatim "bar"]);

    test "two with blank line"
      "{v foo v}\n\n{v bar v}"
      (Ok [`Verbatim "foo"; `Verbatim "bar"]);

    test "followed by words"
      "{v foo v} bar"
      (error 1 10 1 13 ["paragraph must begin on its own line"]);

    test "preceded by words"
      "foo {v bar v}"
      (error 1 4 1 13
        ["'{v ... v}' (verbatim text) must begin on its own line"]);

    test "preceded by paragraph"
      "foo\n{v bar v}"
      (Ok [`Paragraph [`Word "foo"]; `Verbatim "bar"]);

    test "followed by paragraph"
      "{v foo v}\nbar"
      (Ok [`Verbatim "foo"; `Paragraph [`Word "bar"]]);

    test "unterminated"
      "{v foo"
      (error 1 6 1 6
        ["end of text is not allowed in '{v ... v}' (verbatim text)"]);

    test "unterminated, v"
      "{v foo v"
      (error 1 8 1 8
        ["end of text is not allowed in '{v ... v}' (verbatim text)"]);

    test "trailing cr"
      "{v foo\rv}"
      (Ok [`Verbatim "foo\r"]);
  ];



  "shorthand list", [
    test "basic"
      "- foo"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "multiple items"
      "- foo\n- bar"
      (Ok [`List (`Unordered, [
        [`Paragraph [`Word "foo"]];
        [`Paragraph [`Word "bar"]];
      ])]);

    test "two lists"
      "- foo\n\n- bar"
      (Ok [
        `List (`Unordered, [[`Paragraph [`Word "foo"]]]);
        `List (`Unordered, [[`Paragraph [`Word "bar"]]]);
      ]);

    test "ordered"
      "+ foo"
      (Ok [`List (`Ordered, [[`Paragraph [`Word "foo"]]])]);

    test "leading whitespace"
      " - foo"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "trailing whitespace"
      "- foo "
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "no whitespace after bullet"
      "-foo"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "bullet in line"
      "- foo - bar"
      (Ok [`List (`Unordered, [
        [`Paragraph [`Word "foo"; `Space; `Word "-"; `Space; `Word "bar"]]])]);

    test "bullet in line immediately"
      "- - foo"
      (error 1 2 1 3 ["'-' (bulleted list item) must begin on its own line"]);

    test "code block"
      "- {[foo]}"
      (Ok [`List (`Unordered, [[`Code_block "foo"]])]);

    test "verbatim"
      "- {v foo v}"
      (Ok [`List (`Unordered, [[`Verbatim "foo"]])]);

    test "multiple blocks"
      "- foo\n{[bar]}"
      (Ok [`List (`Unordered,
        [[`Paragraph [`Word "foo"]; `Code_block "bar"]])]);

    test "followed by code block"
      "- foo\n\n{[bar]}"
      (Ok [
        `List (`Unordered, [[`Paragraph [`Word "foo"]]]); `Code_block "bar"]);

    test "different kinds"
      "- foo\n+ bar"
      (Ok [
        `List (`Unordered, [[`Paragraph [`Word "foo"]]]);
        `List (`Ordered, [[`Paragraph [`Word "bar"]]]);
      ]);

    test "no content"
      "-"
      (error 1 0 1 1 ["'-' (bulleted list item) cannot be empty"]);

    test "immediate newline"
      "-\nfoo"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "immediate blank line"
      "-\n\nfoo"
      (error 1 0 1 1 ["'-' (bulleted list item) cannot be empty"]);

    test "after code block"
      "{[foo]} - bar"
      (error 1 8 1 9 ["'-' (bulleted list item) must begin on its own line"]);
  ];



  "explicit list", [
    test "basic"
      "{ul {li foo}}"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "ordered"
      "{ol {li foo}}"
      (Ok [`List (`Ordered, [[`Paragraph [`Word "foo"]]])]);

    test "two items"
      "{ul {li foo} {li bar}}"
      (Ok [`List (`Unordered, [
        [`Paragraph [`Word "foo"]];
        [`Paragraph [`Word "bar"]];
      ])]);

    test "items on separate lines"
      "{ul {li foo}\n{li bar}}"
      (Ok [`List (`Unordered, [
        [`Paragraph [`Word "foo"]];
        [`Paragraph [`Word "bar"]];
      ])]);

    test "blank line"
      "{ul {li foo}\n\n{li bar}}"
      (Ok [`List (`Unordered, [
        [`Paragraph [`Word "foo"]];
        [`Paragraph [`Word "bar"]];
      ])]);

    test "blank line in item"
      "{ul {li foo\n\nbar}}"
      (Ok [`List (`Unordered, [
        [`Paragraph [`Word "foo"]; `Paragraph [`Word "bar"]]])]);

    test "junk"
      "{ul foo}"
      (error 1 4 1 7
        [
          "'foo' is not allowed in '{ul ...}' (bulleted list)\n";
          "Suggestion: move 'foo' into a list item, '{li ...}' or '{- ...}'"
        ]);

    test "junk with no whitespace"
      "{ulfoo}"
      (error 1 3 1 6
        [
          "'foo' is not allowed in '{ul ...}' (bulleted list)\n";
          "Suggestion: move 'foo' into a list item, '{li ...}' or '{- ...}'"
        ]);

    test "empty"
      "{ul}"
      (error 1 0 1 3 ["'{ul ...}' (bulleted list) cannot be empty"]);

    test "unterminated list"
      "{ul"
      (error 1 3 1 3
        ["end of text is not allowed in '{ul ...}' (bulleted list)"]);

    test "no whitespace"
      "{ul{li foo}}"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "whitespace at end of item"
      "{ul {li foo\n\n\n}}"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "unterminated {li"
      "{ul {li foo"
      (error 1 11 1 11
        ["end of text is not allowed in '{li ...}' (list item)"]);

    test "unterminated {-"
      "{ul {- foo"
      (error 1 10 1 10
        ["end of text is not allowed in '{- ...}' (list item)"]);

    test "empty {li"
      "{ul {li }}"
      (error 1 4 1 7 ["'{li ...}' (list item) cannot be empty"]);

    test "empty {-"
      "{ul {- }}"
      (error 1 4 1 6 ["'{- ...}' (list item) cannot be empty"]);

    test "{li without whitespace"
      "{ul {lifoo}}"
      (error 1 4 1 7
        ["'{li ...}' must be followed by space, a tab, or a new line"]);

    test "{li followed by newline"
      "{ul {li\nfoo}}"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "{li followed by cr-lf"
      "{ul {li\r\nfoo}}"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "{li followed by blank line"
      "{ul {li\n\nfoo}}"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "{- without whitespace"
      "{ul {-foo}}"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "foo"]]])]);

    test "mixed list items"
      "{ul {li foo} {- bar}}"
      (Ok [`List (`Unordered, [
        [`Paragraph [`Word "foo"]];
        [`Paragraph [`Word "bar"]];
      ])]);

    test "nested"
      "{ul {li {ul {li foo}}}}"
      (Ok [`List (`Unordered,
        [[`List (`Unordered, [[`Paragraph [`Word "foo"]]])]])]);

    test "shorthand in explicit"
      "{ul {li - foo\n- bar}}"
      (Ok [`List (`Unordered, [[`List (`Unordered, [
        [`Paragraph [`Word "foo"]];
        [`Paragraph [`Word "bar"]];
      ])]])]);

    test "explicit in shorthand"
      "- {ul {li foo}}"
      (Ok [`List (`Unordered,
        [[`List (`Unordered, [[`Paragraph [`Word "foo"]]])]])]);

    test "bare {li"
      "{li foo}"
      (error 1 0 1 3
        [
          "'{li ...}' (list item) is not allowed in top-level text\n";
          "Suggestion: move '{li ...}' into '{ul ...}' (bulleted list),";
            " or use '-' (bulleted list item)"
        ]);

    test "bare {-"
      "{- foo"
      (error 1 0 1 2
        [
          "'{- ...}' (list item) is not allowed in top-level text\n";
          "Suggestion: move '{- ...}' into '{ul ...}' (bulleted list),";
            " or use '-' (bulleted list item)"
        ]);

    test "after code block"
      "{[foo]} {ul {li bar}}"
      (error 1 8 1 11
        ["'{ul ...}' (bulleted list) must begin on its own line"]);
  ];



  "heading", [
    test "basic"
      "{2 Foo}"
      (Ok [`Heading (`Section, None, [`Word "Foo"])]);

    test "subsection"
      "{3 Foo}"
      (Ok [`Heading (`Subsection, None, [`Word "Foo"])]);

    test "subsubsection"
      "{4 Foo}"
      (Ok [`Heading (`Subsubsection, None, [`Word "Foo"])]);

    test "bad level (title)"
      "{1 Foo}"
      (error 1 1 1 2 ["'1': bad section level (2-4 allowed)"]);

    test "bad level (too deep)"
      "{5 Foo}"
      (error 1 1 1 2 ["'5': bad section level (2-4 allowed)"]);

    test "bad level (long number)"
      "{22 Foo}"
      (error 1 1 1 3 ["'22': bad section level (2-4 allowed)"]);

    test "leading whitespace"
      "{2  Foo}"
      (Ok [`Heading (`Section, None, [`Word "Foo"])]);

    test "no leading whitespace"
      "{2Foo}"
      (error 1 0 1 2 ["'{2' must be followed by space, a tab, or a new line"]);

    test "no leading whitespace (h3)"
      "{3Foo}"
      (error 1 0 1 2 ["'{3' must be followed by space, a tab, or a new line"]);

    test "leading newline"
      "{2\nFoo}"
      (Ok [`Heading (`Section, None, [`Word "Foo"])]);

    test "leading cr-lf"
      "{2\r\nFoo}"
      (Ok [`Heading (`Section, None, [`Word "Foo"])]);

    test "leading blank line"
      "{2\n\nFoo}"
      (error 2 0 2 0
        ["blank line is not allowed in '{2 ...}' (section heading)"]);

    test "leading blank line (h3)"
      "{3\n\nFoo}"
      (error 2 0 2 0
        ["blank line is not allowed in '{3 ...}' (section heading)"]);

    test "trailing whitespace"
      "{2 Foo }"
      (Ok [`Heading (`Section, None, [`Word "Foo"])]);

    test "trailing newline"
      "{2 Foo\n}"
      (Ok [`Heading (`Section, None, [`Word "Foo"])]);

    test "trailing blank line"
      "{2 Foo\n\n}"
      (error 2 0 2 0
        ["blank line is not allowed in '{2 ...}' (section heading)"]);

    test "nested markup"
      "{2 [foo]}"
      (Ok [`Heading (`Section, None, [`Code_span "foo"])]);

    test "words"
      "{2 foo bar}"
      (Ok [`Heading (`Section, None, [`Word "foo"; `Space; `Word "bar"])]);

    test "nested heading"
      "{2 {2 Foo}}"
      (error 1 3 1 5
        [
          "'{2 ...}' (section heading) is not allowed in";
            " '{2 ...}' (section heading)";
        ]);

    test "in list"
      "- {2 Foo}"
      (error 1 2 1 4
        [
          "'{2 ...}' (section heading) is not allowed in";
            " '-' (bulleted list item)\n";
          "Suggestion: move '{2' outside of any other markup";
        ]);

    test "followed by junk"
      "{2 Foo} bar"
      (error 1 8 1 11 ["paragraph must begin on its own line"]);

    test "preceded by junk"
      "foo {2 Bar}"
      (error 1 4 1 6
        ["'{2 ...}' (section heading) must begin on its own line"]);

    test "followed by block"
      "{2 Foo}\nbar"
      (Ok [`Heading (`Section, None, [`Word "Foo"]); `Paragraph [`Word "bar"]]);

    test "preceded by block"
      "foo\n{2 Bar}"
      (Ok [`Paragraph [`Word "foo"]; `Heading (`Section, None, [`Word "Bar"])]);

    test "label"
      "{2:foo Bar}"
      (Ok [`Heading
        (`Section, Some (Label (dummy_page, "foo")), [`Word "Bar"])]);

    test "whitespace before colon"
      "{2 :foo Bar}"
      (Ok [`Heading (`Section, None, [`Word ":foo"; `Space; `Word "Bar"])]);

    test "whitespace after colon"
      "{2: foo Bar}"
      (error 1 2 1 3 ["heading label cannot be empty"]);

    test "label only"
      "{2:foo}"
      (error 1 0 1 6
        ["'{2 ...}' (section heading) cannot be empty"]);

    test "label only with whitespace"
      "{2:foo }"
      (error 1 0 1 6
        ["'{2 ...}' (section heading) cannot be empty"]);

    test "in list outside item"
      "{ul {2 Foo}}"
      (error 1 4 1 6
        [
          "'{2 ...}' (section heading) is not allowed in";
            " '{ul ...}' (bulleted list)\n";
          "Suggestion: move '{2 ...}' (section heading) outside the list";
        ]);

    test "preceded by shorthand list"
      "- foo\n{2 Bar}"
      (Ok [
        `List (`Unordered, [[`Paragraph [`Word "foo"]]]);
        `Heading (`Section, None, [`Word "Bar"])
      ]);

    test "nested in two lists"
      "{ul {li - foo\n{2 Bar}}}"
      (error 2 0 2 2
        [
          "'{2 ...}' (section heading) is not allowed in";
          " '{li ...}' (list item)\n";
          "Suggestion: move '{2' outside of any other markup";
        ]);
  ];



  "author", [
    test "basic"
      "@author Foo Bar"
      (Ok [`Tag (`Author "Foo Bar")]);

    test "empty"
      "@author"
      (error 1 0 1 7 ["'@author' cannot be empty"]);

    test "whitespace only"
      "@author "
      (error 1 0 1 8 ["'@author' cannot be empty"]);

    test "extra whitespace"
      "@author  Foo Bar "
      (Ok [`Tag (`Author "Foo Bar")]);

    test "newline"
      "@author Foo Bar\n"
      (Ok [`Tag (`Author "Foo Bar")]);

    test "cr-lf"
      "@author Foo Bar\r\n"
      (Ok [`Tag (`Author "Foo Bar")]);

    test "blank line"
      "@author Foo Bar\n\n"
      (Ok [`Tag (`Author "Foo Bar")]);

    test "followed by junk"
      "@author Foo\nbar"
      (error 2 0 2 3
        [
          "paragraph is not allowed in the tags section\n";
          "Suggestion: move 'bar' before any tags";
        ]);

    test "followed by code span"
      "@author Foo\n[bar]"
      (error 2 0 2 5
        [
          "paragraph is not allowed in the tags section\n";
          "Suggestion: move '[...]' (code) before any tags";
        ]);

    test "followed by code block"
      "@author Foo\n{[bar]}"
      (error 2 0 2 7
        [
          "'{[...]}' (code block) is not allowed in the tags section\n";
          "Suggestion: move '{[...]}' (code block) before any tags";
        ]);

    test "followed by verbatim"
      "@author Foo\n{v bar v}"
      (error 2 0 2 9
        [
          "'{v ... v}' (verbatim text) is not allowed in the tags section\n";
          "Suggestion: move '{v ... v}' (verbatim text) before any tags";
        ]);

    test "followed by list"
      "@author Foo\n{ul {li bar}}"
      (error 2 0 2 3
        [
          "'{ul ...}' (bulleted list) is not allowed in the tags section\n";
          "Suggestion: move '{ul ...}' (bulleted list) before any tags";
        ]);

    test "followed by shorthand list"
      "@author Foo\n- bar"
      (error 2 0 2 1
        [
          "'-' (bulleted list item) is not allowed in the tags section\n";
          "Suggestion: move '-' (bulleted list item) before any tags";
        ]);

    test "followed by section heading"
      "@author Foo\n{2 Bar}"
      (error 2 0 2 2
        [
          "'{2 ...}' (section heading) is not allowed in the tags section\n";
          "Suggestion: move '{2 ...}' (section heading) before any tags";
        ]);

    test "followed by author"
      "@author Foo\n@author Bar"
      (Ok [`Tag (`Author "Foo"); `Tag (`Author "Bar")]);

    test "followed by author, cr-lf"
      "@author Foo\n@author Bar"
      (Ok [`Tag (`Author "Foo"); `Tag (`Author "Bar")]);

    test "in author"
      "@author Foo @author Bar"
      (Ok [`Tag (`Author "Foo @author Bar")]);

    test "in author at start"
      "@author @author Foo"
      (Ok [`Tag (`Author "@author Foo")]);

    test "preceded by paragraph"
      "foo\n@author Bar"
      (Ok [`Paragraph [`Word "foo"]; `Tag (`Author "Bar")]);

    test "no markup"
      "@author Foo [Bar]"
      (Ok [`Tag (`Author "Foo [Bar]")]);

    test "in paragraph"
      "foo @author Bar"
      (error 1 4 1 15 ["'@author' must begin on its own line"]);

    test "in code"
      "[@author Foo]"
      (Ok [`Paragraph [`Code_span "@author Foo"]]);

    test "in style"
      "{b @author Foo}"
      (error 1 3 1 15
        ["'@author' is not allowed in '{b ...}' (boldface text)"]);

    test "in heading"
      "{2 @author Foo}"
      (error 1 3 1 15
        ["'@author' is not allowed in '{2 ...}' (section heading)"]);

    test "after shorthand list"
      "- foo\n@author Bar"
      (Ok [`List (
        `Unordered, [[`Paragraph [`Word "foo"]]]);
        `Tag (`Author "Bar")
      ]);

    test "in shorthand list"
      "- foo @author Bar"
      (error 1 6 1 17
        [
          "'@author' is not allowed in '-' (bulleted list item)\n";
          "Suggestion: move '@author' outside of any other markup";
        ]);

    test "in shorthand list at start"
      "- @author Foo"
      (error 1 2 1 13
        [
          "'@author' is not allowed in '-' (bulleted list item)\n";
          "Suggestion: move '@author' outside of any other markup";
        ]);

    test "in shorthand list immediate"
      "-@author Foo"
      (error 1 1 1 12
        [
          "'@author' is not allowed in '-' (bulleted list item)\n";
          "Suggestion: move '@author' outside of any other markup"
        ]);

    test "in list item"
      "{ul {li foo @author Bar}}"
      (error 1 12 1 25
        [
          "'@author' is not allowed in '{li ...}' (list item)\n";
          "Suggestion: move '@author' outside of any other markup";
        ]);

    test "in list item at start"
      "{ul {li @author Foo}}"
      (error 1 8 1 21
        [
          "'@author' is not allowed in '{li ...}' (list item)\n";
          "Suggestion: move '@author' outside of any other markup";
        ]);

    test "in list item on new line"
      "{ul {li foo\n@author Bar}}"
      (error 2 0 2 13
        [
          "'@author' is not allowed in '{li ...}' (list item)\n";
          "Suggestion: move '@author' outside of any other markup";
        ]);

    test "in list"
      "{ul @author Foo}"
      (error 1 4 1 16
        [
          "'@author' is not allowed in '{ul ...}' (bulleted list)\n";
          "Suggestion: move '@author' outside the list";
        ]);

    test "in code block"
      "{[@author Foo]}"
      (Ok [`Code_block "@author Foo"]);

    test "in verbatim"
      "{v @author Foo v}"
      (Ok [`Verbatim "@author Foo"]);

    test "after code block"
      "{[foo]} @author Bar"
      (error 1 8 1 19 ["'@author' must begin on its own line"]);

    test "after verbatim"
      "{v foo v} @author Bar"
      (error 1 10 1 21 ["'@author' must begin on its own line"]);

    test "after heading"
      "{2 Foo} @author Bar"
      (error 1 8 1 19 ["'@author' must begin on its own line"]);

    test "after list"
      "{ul {li foo}} @author Bar"
      (error 1 14 1 25 ["'@author' must begin on its own line"]);

    test "preceded by whitespace"
      " @author Foo Bar"
      (Ok [`Tag (`Author "Foo Bar")]);

    test "second preceded by whitespace"
      "@author Foo\n @author Bar"
      (Ok [`Tag (`Author "Foo"); `Tag (`Author "Bar")]);

    test "prefix"
      "@authorfoo"
      (error 1 0 1 10 ["unknown tag '@authorfoo'"]);
  ];



  "deprecated", [
    test "basic"
      "@deprecated"
      (Ok [`Tag (`Deprecated [])]);

    test "words"
      "@deprecated foo bar"
      (Ok [`Tag (`Deprecated [`Paragraph [`Word "foo"; `Space; `Word "bar"]])]);

    test "multiline"
      "@deprecated foo\nbar"
      (Ok [`Tag (`Deprecated [`Paragraph [`Word "foo"; `Space; `Word "bar"]])]);

    test "paragraphs"
      "@deprecated foo\n\nbar"
      (Ok [`Tag
        (`Deprecated [`Paragraph [`Word "foo"]; `Paragraph [`Word "bar"]])]);

    test "whitespace only"
      "@deprecated "
      (Ok [`Tag (`Deprecated [])]);

    test "immediate newline"
      "@deprecated\nfoo"
      (Ok [`Tag (`Deprecated [`Paragraph [`Word "foo"]])]);

    test "immediate cr-lf"
      "@deprecated\r\nfoo"
      (Ok [`Tag (`Deprecated [`Paragraph [`Word "foo"]])]);

    test "immediate blank line"
      "@deprecated\n\nfoo"
      (Ok [`Tag (`Deprecated [`Paragraph [`Word "foo"]])]);

    test "extra whitespace"
      "@deprecated  foo"
      (Ok [`Tag (`Deprecated [`Paragraph [`Word "foo"]])]);

    test "followed by deprecated"
      "@deprecated foo\n@deprecated bar"
      (Ok [
        `Tag (`Deprecated [`Paragraph [`Word "foo"]]);
        `Tag (`Deprecated [`Paragraph [`Word "bar"]])
      ]);

    test "followed by deprecated, cr-lf"
      "@deprecated foo\r\n@deprecated bar"
      (Ok [
        `Tag (`Deprecated [`Paragraph [`Word "foo"]]);
        `Tag (`Deprecated [`Paragraph [`Word "bar"]])
      ]);

    test "nested in self"
      "@deprecated foo @deprecated bar"
      (error 1 16 1 27
        [
          "'@deprecated' is not allowed in '@deprecated'\n";
          "Suggestion: move '@deprecated' outside of any other markup";
        ]);

    test "nested in self at start"
      "@deprecated @deprecated foo"
      (error 1 12 1 23
        [
          "'@deprecated' is not allowed in '@deprecated'\n";
          "Suggestion: move '@deprecated' outside of any other markup";
        ]);

    test "preceded by paragraph"
      "foo\n@deprecated"
      (Ok [`Paragraph [`Word "foo"]; `Tag (`Deprecated [])]);

    test "preceded by shorthand list"
      "- foo\n@deprecated"
      (Ok [
        `List (`Unordered, [[`Paragraph [`Word "foo"]]]);
        `Tag (`Deprecated [])
      ]);

    test "with shorthand list"
      "@deprecated - foo"
      (Ok [`Tag
        (`Deprecated [`List (`Unordered, [[`Paragraph [`Word "foo"]]])])]);

    test "with shorthand list after newline"
      "@deprecated\n-foo"
      (Ok [`Tag
        (`Deprecated [`List (`Unordered, [[`Paragraph [`Word "foo"]]])])]);

    test "prefix"
      "@deprecatedfoo"
      (error 1 0 1 14 ["unknown tag '@deprecatedfoo'"]);

    test "after code block"
      "{[foo]} @deprecated"
      (error 1 8 1 19 ["'@deprecated' must begin on its own line"]);

    test "followed by section"
      "@deprecated foo\n{2 Bar}"
      (error 2 0 2 2
        [
          "'{2 ...}' (section heading) is not allowed in '@deprecated'\n";
          "Suggestion: move '{2' outside of any other markup"
        ]);
  ];



  "param", [
    test "basic"
      "@param foo"
      (Ok [`Tag (`Param ("foo", []))]);

    test "bare"
      "@param"
      (error 1 0 1 6 ["'@param' expects parameter name on the same line"]);

    test "bare with whitespace"
      "@param "
      (error 1 0 1 6 ["'@param' expects parameter name on the same line"]);

    test "immediate newline"
      "@param\nfoo"
      (error 1 0 1 6 ["'@param' expects parameter name on the same line"]);

    test "followed by whitespace"
      "@param foo "
      (Ok [`Tag (`Param ("foo", []))]);

    test "extra whitespace"
      "@param  foo"
      (Ok [`Tag (`Param ("foo", []))]);

    test "words"
      "@param foo bar baz"
      (Ok [`Tag
        (`Param ("foo", [`Paragraph [`Word "bar"; `Space; `Word "baz"]]))]);

    test "multiline"
      "@param foo\nbar\nbaz"
      (Ok [`Tag
        (`Param ("foo", [`Paragraph [`Word "bar"; `Space; `Word "baz"]]))]);

    test "paragraphs"
      "@param foo bar\n\nbaz"
      (Ok [`Tag (`Param ("foo", [
        `Paragraph [`Word "bar"]; `Paragraph [`Word "baz"]]))]);

    test "two"
      "@param foo\n@param bar"
      (Ok [`Tag (`Param ("foo", [])); `Tag (`Param ("bar", []))]);

    test "nested"
      "@param foo @param bar"
      (error 1 11 1 21
        [
          "'@param' is not allowed in '@param'\n";
          "Suggestion: move '@param' outside of any other markup";
        ]);

    test "preceded by paragraph"
      "foo\n@param bar"
      (Ok [`Paragraph [`Word "foo"]; `Tag (`Param ("bar", []))]);

    test "prefix"
      "@paramfoo"
      (error 1 0 1 9 ["unknown tag '@paramfoo'"]);

    test "after code block"
      "{[foo]} @param foo"
      (error 1 8 1 18 ["'@param' must begin on its own line"]);
  ];



  "raise", [
    test "basic"
      "@raise Foo"
      (Ok [`Tag (`Raise ("Foo", []))]);

    test "bare"
      "@raise"
      (error 1 0 1 6 ["'@raise' expects parameter name on the same line"]);

    test "words"
      "@raise foo bar baz"
      (Ok [`Tag
        (`Raise ("foo", [`Paragraph [`Word "bar"; `Space; `Word "baz"]]))]);

    test "prefix"
      "@raisefoo"
      (error 1 0 1 9 ["unknown tag '@raisefoo'"]);
  ];



  "return", [
    test "basic"
      "@return"
      (Ok [`Tag (`Return [])]);

    test "words"
      "@return foo bar"
      (Ok [`Tag (`Return [`Paragraph [`Word "foo"; `Space; `Word "bar"]])]);

    test "prefix"
      "@returnfoo"
      (error 1 0 1 10 ["unknown tag '@returnfoo'"]);
  ];



  "see", [
    test "url"
      "@see <foo>"
      (Ok [`Tag (`See (`Url, "foo", []))]);

    test "file"
      "@see 'foo'"
      (Ok [`Tag (`See (`File, "foo", []))]);

    test "document"
      "@see \"foo\""
      (Ok [`Tag (`See (`Document, "foo", []))]);

    test "bare"
      "@see"
      (error 1 0 1 4
        ["'@see' must be followed by <url>, 'file', or \"document title\""]);

    test "unterminated url"
      "@see <foo"
      (error 1 0 1 4
        ["'@see' must be followed by <url>, 'file', or \"document title\""]);

    test "unterminated file"
      "@see 'foo"
      (error 1 0 1 4
        ["'@see' must be followed by <url>, 'file', or \"document title\""]);

    test "unterminated document"
      "@see \"foo"
      (error 1 0 1 4
        ["'@see' must be followed by <url>, 'file', or \"document title\""]);

    test "no space"
      "@see<foo>"
      (Ok [`Tag (`See (`Url, "foo", []))]);

    test "words"
      "@see <foo> bar"
      (Ok [`Tag (`See (`Url, "foo", [`Paragraph [`Word "bar"]]))]);

    test "prefix"
      "@seefoo"
      (error 1 0 1 7 ["unknown tag '@seefoo'"]);

    test "after code block"
      "{[foo]} @see <foo>"
      (error 1 8 1 18 ["'@see' must begin on its own line"]);
  ];



  "since", [
    test "basic"
      "@since foo"
      (Ok [`Tag (`Since "foo")]);

    test "bare"
      "@since"
      (error 1 0 1 6 ["'@since' cannot be empty"]);

    test "prefix"
      "@sincefoo"
      (error 1 0 1 9 ["unknown tag '@sincefoo'"]);

    test "with whitespace"
      "@since foo bar"
      (Ok [`Tag (`Since "foo bar")]);

    test "leading whitespace"
      "@since  foo"
      (Ok [`Tag (`Since "foo")]);

    test "trailing whitespace"
      "@since foo "
      (Ok [`Tag (`Since "foo")]);

    test "whitespace only"
      "@since "
      (error 1 0 1 7 ["'@since' cannot be empty"]);
  ];



  "before", [
    test "basic"
      "@before Foo"
      (Ok [`Tag (`Before ("Foo", []))]);

    test "bare"
      "@before"
      (error 1 0 1 7 ["'@before' expects parameter name on the same line"]);

    test "words"
      "@before foo bar baz"
      (Ok [`Tag
        (`Before ("foo", [`Paragraph [`Word "bar"; `Space; `Word "baz"]]))]);

    test "prefix"
      "@beforefoo"
      (error 1 0 1 10 ["unknown tag '@beforefoo'"]);
  ];



  "version", [
    test "basic"
      "@version foo"
      (Ok [`Tag (`Version "foo")]);

    test "bare"
      "@version"
      (error 1 0 1 8 ["'@version' cannot be empty"]);

    test "prefix"
      "@versionfoo"
      (error 1 0 1 11 ["unknown tag '@versionfoo'"]);

    test "with whitespace"
      "@version foo bar"
      (Ok [`Tag (`Version "foo bar")]);

    test "leading whitespace"
      "@version  foo"
      (Ok [`Tag (`Version "foo")]);

    test "trailing whitespace"
      "@version foo "
      (Ok [`Tag (`Version "foo")]);

    test "whitespace only"
      "@version "
      (error 1 0 1 9 ["'@version' cannot be empty"]);
  ];



  "canonical", [
    test "basic"
      "@canonical Foo"
      (Ok [`Tag (`Canonical (Root "Foo", Root ("Foo", TUnknown)))]);

    test "empty"
      "@canonical"
      (error 1 0 1 10 ["'@canonical' cannot be empty"]);

    test "whitespace only"
      "@canonical "
      (error 1 0 1 11 ["'@canonical' cannot be empty"]);

    test "extra whitespace"
      "@canonical  Foo "
      (Ok [`Tag (`Canonical (Root "Foo", Root ("Foo", TUnknown)))]);

    test "prefix"
      "@canonicalfoo"
      (error 1 0 1 13 ["unknown tag '@canonicalfoo'"]);

    (* TODO This should probably be an error of some kind, as Foo Bar is not a
       valid module path. *)
    test "with whitespace"
      "@canonical Foo Bar"
      (Ok [`Tag (`Canonical (Root "Foo Bar", Root ("Foo Bar", TUnknown)))]);
  ];



  "bad markup", [
    test "left brace"
      "{"
      (error 1 0 1 1 ["'{': bad markup"]);

    test "left brace with letter"
      "{g"
      (error 1 0 1 2 ["'{g': bad markup"]);

    test "right brace"
      "}"
      (error 1 0 1 1 ["unpaired '}' (end of markup)"]);

    test "right brace in paragraph"
      "foo}"
      (error 1 3 1 4 ["unpaired '}' (end of markup)"]);

    test "right brace in list item"
      "- foo}"
      (error 1 5 1 6 ["unpaired '}' (end of markup)"]);

    test "right brace in code span"
      "[foo}]"
      (Ok [`Paragraph [`Code_span "foo}"]]);

    test "right brace in code block"
      "{[foo}]}"
      (Ok [`Code_block "foo}"]);

    test "right brace in verbatim text"
      "{v foo} v}"
      (Ok [`Verbatim "foo}"]);

    test "right brace in author"
      "@author Foo}"
      (Ok [`Tag (`Author "Foo}")]);

    test "right brace in deprecated"
      "@deprecated }"
      (error 1 12 1 13 ["unpaired '}' (end of markup)"]);

    test "right bracket"
      "]"
      (error 1 0 1 1 ["unpaired ']' (end of code)"]);

    test "right bracket in paragraph"
      "foo]"
      (error 1 3 1 4 ["unpaired ']' (end of code)"]);

    test "right bracket in shorthand list"
      "- foo]"
      (error 1 5 1 6 ["unpaired ']' (end of code)"]);

    test "right bracket in code span"
      "[]]"
      (error 1 2 1 3 ["unpaired ']' (end of code)"]);

    test "right bracket in style"
      "{b]}"
      (error 1 2 1 3 ["unpaired ']' (end of code)"]);

    test "right bracket in verbatim"
      "{v ] v}"
      (Ok [`Verbatim "]"]);

    test "right bracket in list"
      "{ul ]}"
      (error 1 4 1 5 ["unpaired ']' (end of code)"]);

    test "right bracket in list item"
      "{ul {li ]}}"
      (error 1 8 1 9 ["unpaired ']' (end of code)"]);

    test "right bracket in heading"
      "{2 ]}"
      (error 1 3 1 4 ["unpaired ']' (end of code)"]);

    test "right bracket in author"
      "@author Foo]"
      (Ok [`Tag (`Author "Foo]")]);

    test "at"
      "@"
      (error 1 0 1 1 ["stray '@'"]);

    test "cr"
      "\r"
      (error 1 0 1 1 ["stray '\\r' (carriage return character)"]);

    (* We may actually want to support this instead. *)
    test "email"
      "foo@bar.com"
      (error 1 3 1 7 ["unknown tag '@bar'"]);
  ];



  "utf-8", [
    test "lambda"
      "\xce\xbb"
      (Ok [`Paragraph [`Word "\xce\xbb"]]);

    test "words"
      "\xce\xbb \xce\xbb"
      (Ok [`Paragraph [`Word "\xce\xbb"; `Space; `Word "\xce\xbb"]]);

    test "no validation"
      "\xce"
      (Ok [`Paragraph [`Word "\xce"]]);

    test "escapes"
      "\xce\xbb\\}"
      (Ok [`Paragraph [`Word "\xce\xbb}"]]);

    test "newline"
      "\xce\xbb \n \xce\xbb"
      (Ok [`Paragraph [`Word "\xce\xbb"; `Space; `Word "\xce\xbb"]]);

    test "paragraphs"
      "\xce\xbb \n\n \xce\xbb"
      (Ok [`Paragraph [`Word "\xce\xbb"]; `Paragraph [`Word "\xce\xbb"]]);

    test "code span"
      "[\xce\xbb]"
      (Ok [`Paragraph [`Code_span "\xce\xbb"]]);

    test "minus"
      "\xce\xbb-\xce\xbb"
      (Ok [`Paragraph [`Word "\xce\xbb-\xce\xbb"]]);

    test "shorthand list"
      "- \xce\xbb"
      (Ok [`List (`Unordered, [[`Paragraph [`Word "\xce\xbb"]]])]);

    test "styled"
      "{b \xce\xbb}"
      (Ok [`Paragraph [`Styled (`Bold, [`Word "\xce\xbb"])]]);

    test "reference target"
      "{!\xce\xbb}"
      (Ok [`Paragraph [`Reference (Root ("\xce\xbb", TUnknown), [])]]);

    test "code block"
      "{[\xce\xbb]}"
      (Ok [`Code_block "\xce\xbb"]);

    test "verbatim"
      "{v \xce\xbb v}"
      (Ok [`Verbatim "\xce\xbb"]);

    test "label"
      "{2:\xce\xbb Bar}"
      (Ok [`Heading
        (`Section, Some (Label (dummy_page, "\xce\xbb")), [`Word "Bar"])]);

    test "author"
      "@author \xce\xbb"
      (Ok [`Tag (`Author "\xce\xbb")]);

    test "param"
      "@param \xce\xbb"
      (Ok [`Tag (`Param ("\xce\xbb", []))]);

    test "raise"
      "@raise \xce\xbb"
      (Ok [`Tag (`Raise ("\xce\xbb", []))]);

    test "see"
      "@see <\xce\xbb>"
      (Ok [`Tag (`See (`Url, "\xce\xbb", []))]);

    test "since"
      "@since \xce\xbb"
      (Ok [`Tag (`Since "\xce\xbb")]);

    test "before"
      "@before \xce\xbb"
      (Ok [`Tag (`Before ("\xce\xbb", []))]);

    test "version"
      "@version \xce\xbb"
      (Ok [`Tag (`Version "\xce\xbb")]);

    test "right brace"
      "\xce\xbb}"
      (error 1 2 1 3 ["unpaired '}' (end of markup)"]);
  ];



  "comment location", [
    test "error on first line" ~comment_location:{line = 2; column = 4}
      "  @foo"
      (error 2 6 2 10 ["unknown tag '@foo'"]);

    test "error on second line" ~comment_location:{line = 2; column = 4}
      "  \n  @foo"
      (error 3 2 3 6 ["unknown tag '@foo'"]);
  ];



  "unsupported", [
    (* test "module index"
      "{!modules:foo}"
      (Ok []); *)

    (* test "index list"
      "{!indexlist}"
      (Ok []); *)

    test "left alignment"
      "{L foo}"
      (error 1 0 1 2 ["'{L': bad markup"]);

    test "center alignment"
      "{C foo}"
      (error 1 0 1 2 ["'{C': bad markup"]);

    test "right alignment"
      "{R foo}"
      (error 1 0 1 2 ["'{R': bad markup"]);

    test "target-specific code"
      "{%foo%}"
      (error 1 0 1 2 ["'{%': bad markup"]);

    test "custom style"
      "{c foo}"
      (error 1 0 1 2 ["'{c': bad markup"]);

    test "custom tag"
      "@custom"
      (error 1 0 1 7 ["unknown tag '@custom'"]);

    (* test "custom reference kind"
      "{!custom:foo}"
      (Ok []); *)

    test "html tag"
      "<b>foo</b>"
      (Ok [`Paragraph [`Word "<b>foo</b>"]]);
  ];
]



let () =
  Alcotest.run "parser" tests
