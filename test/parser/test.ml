type test_case = {
  name : string;
  parser_input : string;
  sections_allowed : Parser_.sections_allowed;
  location : Model.Location_.point;
}

let make_test_case
    ?(sections_allowed = `No_titles)
    ?(location = {Model.Location_.line = 1; column = 0})
    name
    parser_input =
  {name; parser_input; sections_allowed; location}

let t = make_test_case

type test_suite = string * (test_case list)



let tests : test_suite list = [
  "trivial", [
    t "empty" "";
    t "space" " ";
    t "two-spaces" "  ";
    t "tab" "\t";
    t "mixed-space" " \t \t";
    t "newline" "\n";
    t "blank-line" "\n\n";
    t "cr-lf" "\r\n";
  ];

  "one-paragraph", [
    t "word" "foo";
    t "two-words" "foo bar";
    t "two-spaces" "foo  bar";
    t "mixed-space" "foo \t \t bar";
    t "two-lines" "foo\nbar";
    t "two-lines-cr-lf" "foo\r\nbar";
    t "leading-space" " foo";
    t "trailing-space" "foo ";
    t "leading-space-on-line" "foo\n bar";
    t "trailing-space-on-line" "foo \nbar";
    t "leading-tab-on-line" "foo\n\tbar";
    t "trailing-tab-on-line" "foo\t\nbar";
    t "email" "foo@bar.com";
  ];

  "two-paragraphs", [
    t "basic" "foo\n\nbar";
    t "trailing-space" "foo \n\nbar";
    t "leading-space" "foo\n\n bar";
    t "cr-lf" "foo\r\n\r\nbar";
    t "mixed-cr-lf" "foo\n\r\nbar";
  ];

  "plus-minus-words", [
    t "minus-in-word" "foo-bar";
    t "minus-as-word" "foo -";
    t "plus-in-word" "foo+bar";
    t "plus-as-word" "foo +";
    t "negative-number" "-3.14 -1337";
    t "en-em-dash" "-- ---";
    t "minus-at" "-@";
    t "at-minus" "-@-";
    t "option" "--option";
  ];

  "escape-sequence", [
    t "left-brace" "\\{";
    t "left-brace-in-word" "foo\\{bar";
    t "right-brace" "\\}";
    t "right-brace-in-word" "foo\\}bar";
    t "left-bracket" "\\[";
    t "left-bracket-in-word" "foo\\[bar";
    t "right-bracket" "\\]";
    t "right-bracket-in-word" "foo\\]bar";
    t "at" "\\@";
    t "not-a-tag" "\\@author";
    t "at-in-word" "foo\\@bar";
    t "trailing-backslash" "foo\\";
    t "non-escape" "foo\\bar";
    t "backslash-not-escaped" "foo\\\\{bar";
    t "single-backslash" "\\";
    t "escape-minus" "\\{- foo";
    t "escape-plus" "\\{+ foo";
    t "minus-escape" "-\\{";
    t "plus-escape" "+\\{";
    t "escape-at" "\\{@author";
    t "two" "\\{\\}";
  ];

  "code-span", [
    t "basic" "[foo]";
    t "empty" "[]";
    t "list" "[[]]";
    (* TODO The next two error messages are particularly unintuitive. *)
    t "unbalanced-list" "[[]";
    t "no-markup" "[{b]";
    t "few-escapes" "[\\{]";
    t "escaped-right-bracket" "[\\]]";
    t "escaped-left-bracket" "[\\[]";
    t "whitespace-preserved" "[ foo  bar ]";
    t "no-newlines" "[foo\nbar]";
    t "cr-lf-preserved" "[foo\r\nbar]";
    t "no-double-newline" "[foo\n\nbar]";
    t "no-double-crlf" "[foo\r\n\r\nbar]";
    t "not-merged" "[foo][bar]";
    t "explicit-space" "[foo] [bar]";
    t "unterminated" "[foo";
  ];

  "bold", [
    t "basic" "{b foo}";
    t "extra-leading-whitespace" "{b  \t foo}";
    t "leading-newline" "{b\nfoo}";
    t "leading-cr-lf" "{b\r\nfoo}";
    t "leading-newline-and-whitespace" "{b\n foo}";
    t "no-leading-whitespace" "{bfoo}";
    t "trailing-whitespace" "{b foo }";
    t "trailing-newline" "{b foo\n}";
    t "trailing-cr-lf" "{b foo\r\n}";
    t "two-words" "{b foo bar}";
    t "not-merged" "{b foo}{b bar}";
    t "nested" "{b foo{b bar}}";
    t "newline" "{b foo\nbar}";
    t "cr-lf" "{b foo\r\nbar}";
    t "minus" "{b -}";
    t "minus-list-item" "{b foo\n - bar}";
    t "plus-list-item" "{b foo\n + bar}";
    t "immediate-minus-list-item" "{b\n- foo}";
    t "immediate-plus-list-item" "{b\n+ foo}";
    t "blank-line" "{b foo\n\nbar}";
    t "immediate-blank-line" "{b\n\n";
    t "end-of-comment" "{b foo";
    t "nested-code-block" "{b {[foo]}";
    t "degenerate" "{b}";
    t "empty" "{b }";
  ];

  "italic", [
    t "basic" "{i foo}";
    t "extra-leading-whitespace" "{i  \t foo}";
    t "leading-newline" "{i\nfoo}";
    t "leading-newline-and-whitespace" "{i\n foo}";
  ];

  "emphasis", [
    t "basic" "{e foo}";
    t "extra-leading-whitespace" "{e  \t foo}";
    t "leading-newline" "{e\nfoo}";
    t "leading-newline-and-whitespace" "{e\n foo}";
  ];

  "superscript", [
    t "basic" "{^ foo}";
    t "extra-leading-whitespace" "{^  \t foo}";
    t "leading-newline" "{^\nfoo}";
    t "leading-cr-lf" "{^\r\nfoo}";
    t "leading-newline-and-whitespace" "{^\n foo}";
    t "no-whitespace" "{^foo}";
    t "degenerate" "{^}";
    t "empty" "{^ }";
  ];

  "subscript", [
    t "basic" "{_ foo}";
    t "extra-leading-whitespace" "{_  \t foo}";
    t "leading-newline" "{_\nfoo}";
    t "leading-newline-and-whitespace" "{_\n foo}";
    t "no-whitespace" "{_foo}";
    t "v-verbose" "{_uv}";
  ];

  "simple-reference", [
    t "basic" "{!foo}";
    t "leading-whitespace" "{! foo}";
    t "trailing-whitespace" "{!foo }";
    t "adjacent-word-leading" "bar{!foo}";
    t "explicit-leading-space" "bar {!foo}";
    t "adjacent-word-trailing" "{!foo}bar";
    t "explicit-trailing-space" "{!foo} bar";
    t "kind" "{!val:foo}";
    t "empty" "{!}";
    t "whitespace-only" "{! }";
    t "internal-whitespace" "{!( * )}";
    (* TODO Limiting the character combinations allowed will make it easier to
       catch expressions accidentally written inside references. This can also
       be caught by a good resolver and resolver error messages. *)
    (* t "expression" *)
    t "unterminated" "{!foo";
    t "internal-whitespace-in-kind" "{!va l:foo}";
    t "internal-whitespace-in-referent" "{!val:( * )}";
    t "two-colons" "{!val:foo:bar}";
    t "space-before-colon" "{!val :foo}";
    t "space-after-colon" "{!val: foo}";
    t "unterminated-after-kind" "{!val:foo";
    t "operator" "{!(>>=)}";
    t "operator-with-dash" "{!(@->)}";
  ];

  "reference-with-text", [
    t "basic" "{{!foo} bar}";
    t "degenerate" "{{!foo}}";
    t "empty" "{{!foo} }";
    t "nested-markup" "{{!foo} {b bar}}";
    t "in-markup" "{e {{!foo} bar}}";
    t "no-separating-space" "{{!foo}bar}";
    t "kind" "{{!val:foo} bar}";
    t "nested-reference" "{{!foo} {!bar}}";
    t "nested-empty" "{{!foo} {{!bar}}}";
    t "nested-through-emphasis" "{{!foo} {e {{!bar} baz}}}";
    t "simple-through-emphasis" "{{!foo} {e {!bar}}}";
    t "empty-target" "{{!} foo}";
    t "whitespace-only-in-target" "{{! } foo}";
    t "internal-whitespace" "{{!( * )} baz}";
    t "unterminated" "{{!foo";
    t "unterminated-content" "{{!foo} bar";
  ];

  "link", [
    t "basic" "{{:foo} bar}";
    t "nested-markup" "{{:foo} {b bar}}";
    t "in-markup" "{e {{:foo} bar}}";
    t "no-separating-space" "{{:foo}bar}";
    t "nested-link" "{{:foo} {{:bar} baz}}";
    t "nested-through-emphasis" "{{:foo} {e {{:bar} baz}}}";
    t "reference-through-emphasis" "{{:foo} {e {!bar}}}";
    t "nested-in-reference" "{{!foo} {e {{:bar} baz}}}";
    t "empty-target" "{{:} foo}";
    t "whitespace-only-in-target" "{{: } foo}";
    t "empty" "{{:foo}}";
    t "internal-whitespace" "{{:foo bar} baz}";
    t "unterminated" "{{:foo";
    t "single-braces" "{:foo}";
    t "unterminated-single-braces" "{:foo";
    t "empty-single-braces" "{:}";
    t "single-braces-whitespace-only" "{: }";
  ];

  "raw-markup", [
    t "html-target" "{%html:foo%}";
    t "whitespace" "{%html: foo bar %}";
    t "whitespace-only" "{%html: %}";
    t "empty" "{%html:%}";
    t "html-payload" "{%html:<e>foo</e>%}";
    t "colon" "{%html:foo:bar%}";
    t "no-target" "{%foo%}";
    t "empty-target" "{%:foo%}";
    t "whitespace-target" "{% :foo%}";
    t "invalid-target" "{%xml:foo%}";
    t "incorrect-case-target" "{%HTML:foo%}";
    t "multiline-target" "{%\n:foo%}";
    t "percent-in-target" "{%%:%}";
    t "percent-in-payload" "{%html:%%}";
    t "multiple-percent-in-target" "{%%%foo%%:%}";
    t "multiple-percent-in-payload" "{%html:%%foo%%%}";
    t "opener-in-target" "{%{%:foo%}";
    t "opener-in-payload" "{%html:{%%}";
    t "right-brace-in-target" "{%}:%}";
    t "right-brace-in-payload" "{%html:}%}";
    t "unterminated" "{%";
    t "unterminated-after-target" "{%html:";
    t "degenerate" "{%}";
  ];

  "module-list", [
    t "basic" "{!modules:Foo}";
    t "two" "{!modules:Foo Bar}";
    t "extra-whitespace" "{!modules: Foo  Bar }";
    t "newline" "{!modules:Foo\nBar}";
    t "cr-lf" "{!modules:Foo\r\nBar}";
    t "empty" "{!modules:}";
    t "whitespace-only" "{!modules: }";
    t "unterminated" "{!modules:";
    t "in-paragraph" "foo {!modules:Foo}";
    t "followed-by-word" "{!modules:Foo} foo";
    t "in-list" "- {!modules:Foo}";
  ];

  "code-block", [
    t "basic" "{[foo]}";
    t "empty" "{[]}";
    t "whitespace-only" "{[ ]}";
    t "blank-line-only" "{[\n  \n]}";
    t "whitespace" "{[foo bar]}";
    t "newline" "{[foo\nbar]}";
    t "cr-lf" "{[foo\r\nbar]}";
    t "blank-line" "{[foo\n\nbar]}";
    t "leading-whitespace" "{[ foo]}";
    t "leading-whitespace-two" "{[ foo\n bar]}";
    t "leading-whitespace-two-cr-lf" "{[ foo\r\n bar]}";
    t "leading-whitespace-two-different-indent" "{[ foo\n   bar]}";
    t "leading-whitespace-two-different-indent-rev" "{[   foo\n bar]}";
    t "leading-whitespace-with-empty-line" "{[ foo\n\n bar]}";
    t "leading-whitespace-with-whitespace-line-short" "{[  foo\n \n  bar]}";
    t "leading-whitespace-with-whitespace-line-long" "{[ foo\n   \n bar]}";
    t "leading-tab" "{[\tfoo]}";
    t "leading-tab-two" "{[\tfoo\n\tbar]}";
    t "leading-tab-two-different-indent" "{[\tfoo\n\t\tbar]}";
    t "leading-newline" "{[\nfoo]}";
    t "leading-cr-lf" "{[\r\nfoo]}";
    t "leading-newlines" "{[\n\nfoo]}";
    t "leading-newline-with-space" "{[\n foo]}";
    t "leading-newline-with-trash" "{[ \nfoo]}";
    t "nested-opener" "{[{[]}";
    t "nested-closer" "{[foo]}]}";
    t "nested-bracket" "{[]]}";
    t "two-nested-brackets" "{[]]]}";
    t "nested-brackets-in-text" "{[foo]]bar]}";
    t "trailing-whitespace" "{[foo ]}";
    t "trailing-tab" "{[foo\t]}";
    t "trailing-newline" "{[foo\n]}";
    t "trailing-cr-lf" "{[foo\r\n]}";
    t "trailing-newlines" "{[foo\n\n]}";
    t "preceded-by-whitespace" " {[foo]}";
    t "followed-by-whitespace" "{[foo]} ";
    t "two-on-one-line" "{[foo]} {[bar]}";
    t "two" "{[foo]}\n{[bar]}";
    t "two-with-blank-line" "{[foo]}\n\n{[bar]}";
    t "followed-by-words" "{[foo]} bar";
    t "preceded-by-words" "foo {[bar]}";
    t "preceded-by-paragraph" "foo\n{[bar]}";
    t "followed-by-paragraph" "{[foo]}\nbar";
    t "unterminated" "{[foo";
    t "unterminated-bracket" "{[foo]";
    t "trailing-cr" "{[foo\r]}";
  ];

  "verbatim", [
    t "basic" "{v foo v}";
    t "empty" "{v v}";
    t "degenerate" "{vv}";
    t "whitespace-only" "{v  v}";
    t "blank-line-only" "{v\n  \nv}";
    t "no-leading-whitespace" "{vfoo v}";
    t "no-trailing-whitespace" "{v foov}";
    t "multiple-leading-whitespace" "{v  foo v}";
    t "multiple-trailing-whitespace" "{v foo  v}";
    t "leading-tab" "{v\tfoo v}";
    t "leading-newline" "{v\nfoo v}";
    t "leading-cr-lf" "{v\r\nfoo v}";
    t "trailing-tab" "{v foo\tv}";
    t "trailing-newline" "{v foo\nv}";
    t "trailing-cr-lf" "{v foo\r\nv}";
    t "internal-whitespace" "{v foo bar v}";
    t "newline" "{v foo\nbar v}";
    t "cr-lf" "{v foo\r\nbar v}";
    t "blank-line" "{v foo\n\nbar v}";
    t "leading-newlines" "{v\n\nfoo v}";
    t "leading-newline-with-space" "{v\n foo v}";
    t "leading-newline-with-trash" "{v \nfoo v}";
    t "nested-opener" "{v {v v}";
    t "nested-closer" "{v foo v} v}";
    t "nested-closer-with-word" "{v {dev} v}";
    t "nested-v" "{v v v}";
    t "two-nested-vs" "{v vv v}";
    t "nested-v-at-end" "{v vv}";
    t "two-nested-vs-at-end" "{v vvv}";
    t "nested-vs-in-text" "{v foovvbar v}";
    t "trailing-newlines" "{v foo\n\nv}";
    t "preceded-by-whitespace" " {v foo v}";
    t "followed-by-whitespace" "{v foo v} ";
    t "two-on-one-line" "{v foo v} {v bar v}";
    t "two" "{v foo v}\n{v bar v}";
    t "two-with-blank-line" "{v foo v}\n\n{v bar v}";
    t "followed-by-words" "{v foo v} bar";
    t "preceded-by-words" "foo {v bar v}";
    t "preceded-by-paragraph" "foo\n{v bar v}";
    t "followed-by-paragraph" "{v foo v}\nbar";
    t "unterminated" "{v foo";
    t "unterminated-v" "{v foo v";
    t "trailing-cr" "{v foo\rv}";
  ];

  "shorthand-list", [
    t "basic" "- foo";
    t "multiple-items" "- foo\n- bar";
    t "two-lists" "- foo\n\n- bar";
    t "ordered" "+ foo";
    t "leading-whitespace" " - foo";
    t "trailing-whitespace" "- foo ";
    t "bullet-in-line" "- foo - bar";
    t "bullet-in-line-immediately" "- - foo";
    t "code-block" "- {[foo]}";
    t "verbatim" "- {v foo v}";
    t "multiple-blocks" "- foo\n{[bar]}";
    t "followed-by-code-block" "- foo\n\n{[bar]}";
    t "different-kinds" "- foo\n+ bar";
    t "no-content" "-";
    t "immediate-newline" "-\nfoo";
    t "immediate-blank-line" "-\n\nfoo";
    t "immediate-markup" "-{b foo}";
    t "after-code-block" "{[foo]} - bar";
  ];

  "explicit-list", [
    t "basic" "{ul {li foo}}";
    t "ordered" "{ol {li foo}}";
    t "two-items" "{ul {li foo} {li bar}}";
    t "items-on-separate-lines" "{ul {li foo}\n{li bar}}";
    t "blank-line" "{ul {li foo}\n\n{li bar}}";
    t "blank-line-in-item" "{ul {li foo\n\nbar}}";
    t "junk" "{ul foo}";
    t "junk-with-no-whitespace" "{ulfoo}";
    t "empty" "{ul}";
    t "unterminated-list" "{ul";
    t "no-whitespace" "{ul{li foo}}";
    t "whitespace-at-end-of-item" "{ul {li foo\n\n\n}}";
    t "unterminated-{li" "{ul {li foo";
    t "unterminated-{-" "{ul {- foo";
    t "empty-{li" "{ul {li }}";
    t "empty-{-" "{ul {- }}";
    t "{li-without-whitespace" "{ul {lifoo}}";
    t "{li-followed-by-newline" "{ul {li\nfoo}}";
    t "{li-followed-by-cr-lf" "{ul {li\r\nfoo}}";
    t "{li-followed-by-blank-line" "{ul {li\n\nfoo}}";
    t "{--without-whitespace" "{ul {-foo}}";
    t "mixed-list-items" "{ul {li foo} {- bar}}";
    t "nested" "{ul {li {ul {li foo}}}}";
    t "shorthand-in-explicit" "{ul {li - foo\n- bar}}";
    t "explicit-in-shorthand" "- {ul {li foo}}";
    t "bare-{li" "{li foo}";
    t "bare-{-" "{- foo";
    t "after-code-block" "{[foo]} {ul {li bar}}";
  ];

  "heading", [
    t "basic" "{2 Foo}";
    t "subsection" "{3 Foo}";
    t "subsubsection" "{4 Foo}";
    t "leading-whitespace" "{2  Foo}";
    t "no-leading-whitespace" "{2Foo}";
    t "no-leading-whitespace-h3" "{3Foo}";
    t "leading-newline" "{2\nFoo}";
    t "leading-cr-lf" "{2\r\nFoo}";
    t "leading-blank-line" "{2\n\nFoo}";
    t "leading-blank-line-h3" "{3\n\nFoo}";
    t "trailing-whitespace" "{2 Foo }";
    t "trailing-newline" "{2 Foo\n}";
    t "trailing-blank-line" "{2 Foo\n\n}";
    t "nested-markup" "{2 [foo]}";
    t "nested-code-with-uppercase" "{2 [Foo]}";
    t "nested-code-with-spaces" "{2 [ foo bar  baz  \t]}";
    t "nested-code-with-newline" "{2 [foo\nbar\r\nbaz]}";
    t "nested-style" "{2 {e foo bar}}";
    t "words" "{2 foo bar}";
    t "nested-heading" "{2 {2 Foo}}";
    t "in-list" "- {2 Foo}";
    t "followed-by-junk" "{2 Foo} bar";
    t "preceded-by-junk" "foo {2 Bar}";
    t "followed-by-block" "{2 Foo}\nbar";
    t "preceded-by-block" "foo\n{2 Bar}";
    t "label" "{2:foo Bar}";
    t "whitespace-before-colon" "{2 :foo Bar}";
    t "whitespace-after-colon" "{2: foo Bar}";
    t "label-only" "{2:foo}";
    t "label-only-with-whitespace" "{2:foo }";
    t "in-list-outside-item" "{ul {2 Foo}}";
    t "preceded-by-shorthand-list" "- foo\n{2 Bar}";
    t "nested-in-two-lists" "{ul {li - foo\n{2 Bar}}}";
    t "bad-level-long-number" "{22 Foo}";
    t "bad-level-long-number-with-label" "{22:foo Bar}";
    t "bad-level-leading-zero" "{02 Foo}";
    t "bad-level-leading-zero-with-label" "{02:foo Bar}";
    t "bad-level-title" "{0 Foo}";
    t "bad-level-too-deep" "{6 Foo}";
    t "link-in-markup" "{2 {{:foo}}}";
    t "reference-in-markup" "{2 {!foo}}";
    t "two" "{2 Foo}\n{2 Bar}";
    t "greater" "{2 Foo}\n{3 Bar}";
  ];

  "section-contexts", [
    t "titles-allowed" "{0 Foo}"
      ~sections_allowed:`All;
    t "titles-no-high-levels" "{6 Foo}"
      ~sections_allowed:`All;
    t "two-titles" "{0 Foo}\n{0 Bar}"
      ~sections_allowed:`All;
    t "no-heading" "foo"
      ~sections_allowed:`All;
    t "heading-after-paragraph" "foo\n{0 Bar}"
      ~sections_allowed:`All;
    t "two-top-level-section-headings" "{1 Foo}\n{1 Bar}"
      ~sections_allowed:`All;
    t "two-headings-second-higher" "{1 Foo}\n{0 Bar}"
      ~sections_allowed:`All;
    t "three-headings-last-two-higher" "{3 Foo}\n{1 Bar}\n{2 Baz}"
      ~sections_allowed:`All;
    t "none" "{1 Foo}"
      ~sections_allowed:`None;
    t "title-no-titles-allowed" "{0 Foo}"
      ~sections_allowed:`No_titles;
    t "two-titles-none-allowed" "{0 Foo}\n{0 Bar}"
      ~sections_allowed:`No_titles;
    t "two-headings-none-allowed" "{1 Foo}\n{1 Bar}"
      ~sections_allowed:`None;
    t "multiple-with-bad-section" "{0 Foo}\n{0 Foo}\n{6 Foo}"
      ~sections_allowed:`All;
    t "promoted-duplicates" "{6 Foo}\n{6 Bar}"
      ~sections_allowed:`All;
    t "section-promoted-to-duplicate" "{5 Foo}\n{6 Bar}"
      ~sections_allowed:`All;
  ];

  "author", [
    t "basic" "@author Foo Bar";
    t "empty" "@author";
    t "whitespace-only" "@author ";
    t "extra-whitespace" "@author  Foo Bar ";
    t "newline" "@author Foo Bar\n";
    t "cr-lf" "@author Foo Bar\r\n";
    t "blank-line" "@author Foo Bar\n\n";
    t "followed-by-junk" "@author Foo\nbar";
    t "followed-by-code-span" "@author Foo\n[bar]";
    t "followed-by-code-block" "@author Foo\n{[bar]}";
    t "followed-by-verbatim" "@author Foo\n{v bar v}";
    t "followed-by-modules" "@author foo\n{!modules:Foo}";
    t "followed-by-list" "@author Foo\n{ul {li bar}}";
    t "followed-by-shorthand-list" "@author Foo\n- bar";
    t "followed-by-section-heading" "@author Foo\n{2 Bar}";
    t "followed-by-author" "@author Foo\n@author Bar";
    t "followed-by-author-cr-lf" "@author Foo\n@author Bar";
    t "in-author" "@author Foo @author Bar";
    t "in-author-at-start" "@author @author Foo";
    t "preceded-by-paragraph" "foo\n@author Bar";
    t "no-markup" "@author Foo [Bar]";
    t "in-paragraph" "foo @author Bar";
    t "in-code" "[@author Foo]";
    t "in-style" "{b @author Foo}";
    t "in-heading" "{2 @author Foo}";
    t "after-shorthand-list" "- foo\n@author Bar";
    t "in-shorthand-list" "- foo @author Bar";
    t "in-shorthand-list-at-start" "- @author Foo";
    t "in-list-item" "{ul {li foo @author Bar}}";
    t "in-list-item-at-start" "{ul {li @author Foo}}";
    t "in-list-item-on-new-line" "{ul {li foo\n@author Bar}}";
    t "in-list" "{ul @author Foo}";
    t "in-code-block" "{[@author Foo]}";
    t "in-verbatim" "{v @author Foo v}";
    t "after-code-block" "{[foo]} @author Bar";
    t "after-verbatim" "{v foo v} @author Bar";
    t "after-heading" "{2 Foo} @author Bar";
    t "after-list" "{ul {li foo}} @author Bar";
    t "preceded-by-whitespace" " @author Foo Bar";
    t "second-preceded-by-whitespace" "@author Foo\n @author Bar";
    t "prefix" "@authorfoo";
  ];

  "deprecated", [
    t "basic" "@deprecated";
    t "words" "@deprecated foo bar";
    t "multiline" "@deprecated foo\nbar";
    t "paragraphs" "@deprecated foo\n\nbar";
    t "whitespace-only" "@deprecated ";
    t "immediate-newline" "@deprecated\nfoo";
    t "immediate-cr-lf" "@deprecated\r\nfoo";
    t "immediate-blank-line" "@deprecated\n\nfoo";
    t "extra-whitespace" "@deprecated  foo";
    t "followed-by-deprecated" "@deprecated foo\n@deprecated bar";
    t "followed-by-deprecated-cr-lf" "@deprecated foo\r\n@deprecated bar";
    t "nested-in-self" "@deprecated foo @deprecated bar";
    t "nested-in-self-at-start" "@deprecated @deprecated foo";
    t "preceded-by-paragraph" "foo\n@deprecated";
    t "preceded-by-shorthand-list" "- foo\n@deprecated";
    t "with-shorthand-list" "@deprecated - foo";
    t "with-shorthand-list-after-newline" "@deprecated\n- foo";
    t "prefix" "@deprecatedfoo";
    t "after-code-block" "{[foo]} @deprecated";
    t "followed-by-section" "@deprecated foo\n{2 Bar}";
  ];

  "param", [
    t "basic" "@param foo";
    t "bare" "@param";
    t "bare-with-whitespace" "@param ";
    t "immediate-newline" "@param\nfoo";
    t "followed-by-whitespace" "@param foo ";
    t "extra-whitespace" "@param  foo";
    t "words" "@param foo bar baz";
    t "multiline" "@param foo\nbar\nbaz";
    t "paragraphs" "@param foo bar\n\nbaz";
    t "two" "@param foo\n@param bar";
    t "nested" "@param foo @param bar";
    t "preceded-by-paragraph" "foo\n@param bar";
    t "prefix" "@paramfoo";
    t "after-code-block" "{[foo]} @param foo";
  ];

  "raise", [
    t "basic" "@raise Foo";
    t "bare" "@raise";
    t "words" "@raise foo bar baz";
    t "prefix" "@raisefoo";
  ];

  "return", [
    t "basic" "@return";
    t "words" "@return foo bar";
    t "prefix" "@returnfoo";
  ];

  "see", [
    t "url" "@see <foo>";
    t "file" "@see 'foo'";
    t "document" "@see \"foo\"";
    t "bare" "@see";
    t "unterminated-url" "@see <foo";
    t "unterminated-file" "@see 'foo";
    t "unterminated-document" "@see \"foo";
    t "no-space" "@see<foo>";
    t "words" "@see <foo> bar";
    t "prefix" "@seefoo";
    t "after-code-block" "{[foo]} @see <foo>";
  ];

  "since", [
    t "basic" "@since foo";
    t "bare" "@since";
    t "prefix" "@sincefoo";
    t "with-whitespace" "@since foo bar";
    t "leading-whitespace" "@since  foo";
    t "trailing-whitespace" "@since foo ";
    t "whitespace-only" "@since ";
  ];

  "before", [
    t "basic" "@before Foo";
    t "bare" "@before";
    t "words" "@before foo bar baz";
    t "prefix" "@beforefoo";
  ];

  "version", [
    t "basic" "@version foo";
    t "bare" "@version";
    t "prefix" "@versionfoo";
    t "with-whitespace" "@version foo bar";
    t "leading-whitespace" "@version  foo";
    t "trailing-whitespace" "@version foo ";
    t "whitespace-only" "@version ";
  ];

  "canonical", [
    t "basic" "@canonical Foo";
    t "empty" "@canonical";
    t "whitespace-only" "@canonical ";
    t "extra-whitespace" "@canonical  Foo ";
    t "prefix" "@canonicalfoo";
    (* TODO This should probably be an error of some kind, as Foo Bar is not a
       valid module path. *)
    t "with-whitespace" "@canonical Foo Bar";
  ];

  "inline", [
    t "basic" "@inline";
    t "prefix" "@inlinefoo";
    t "extra-whitespace" "@inline ";
    t "followed-by-junk" "@inline foo";
    t "followed-by-paragraph" "@inline\nfoo";
    t "followed-by-tag" "@inline\n@deprecated";
    t "with-list" "@inline - foo";
  ];

  "open", [
    t "basic" "@open";
    t "prefix" "@openfoo";
    t "extra-whitespace" "@open ";
    t "followed-by-junk" "@open foo";
    t "followed-by-paragraph" "@open\nfoo";
    t "followed-by-tag" "@open\n@deprecated";
    t "with-list" "@open - foo";
  ];

  "closed", [
    t "basic" "@closed";
    t "prefix" "@closedfoo";
    t "extra-whitespace" "@closed ";
    t "followed-by-junk" "@closed foo";
    t "followed-by-paragraph" "@closed\nfoo";
    t "followed-by-tag" "@closed\n@deprecated";
    t "with-list" "@closed - foo";
  ];

  "bad-markup", [
    t "left-brace" "{";
    t "left-brace-with-letter" "{g";
    t "braces-instead-of-brackets" "{foo}";
    t "right-brace" "}";
    t "right-brace-in-paragraph" "foo}";
    t "right-brace-in-list-item" "- foo}";
    t "right-brace-in-code-span" "[foo}]";
    t "right-brace-in-code-block" "{[foo}]}";
    t "right-brace-in-verbatim-text" "{v foo} v}";
    t "right-brace-in-author" "@author Foo}";
    t "right-brace-in-deprecated" "@deprecated }";
    t "right-bracket" "]";
    t "right-bracket-in-paragraph" "foo]";
    t "right-bracket-in-shorthand-list" "- foo]";
    t "right-bracket-in-code-span" "[]]";
    t "right-bracket-in-style" "{b]}";
    t "right-bracket-in-verbatim" "{v ] v}";
    t "right-bracket-in-list" "{ul ]}";
    t "right-bracket-in-list-item" "{ul {li ]}}";
    t "right-bracket-in-heading" "{2 ]}";
    t "right-bracket-in-author" "@author Foo]";
    t "at" "@";
    t "cr" "\r";
  ];

  "utf-8", [
    t "lambda" "\xce\xbb";
    t "words" "\xce\xbb \xce\xbb";
    t "no-validation" "\xce";
    t "escapes" "\xce\xbb\\}";
    t "newline" "\xce\xbb \n \xce\xbb";
    t "paragraphs" "\xce\xbb \n\n \xce\xbb";
    t "code-span" "[\xce\xbb]";
    t "minus" "\xce\xbb-\xce\xbb";
    t "shorthand-list" "- \xce\xbb";
    t "styled" "{b \xce\xbb}";
    t "reference-target" "{!\xce\xbb}";
    t "code-block" "{[\xce\xbb]}";
    t "verbatim" "{v \xce\xbb v}";
    t "label" "{2:\xce\xbb Bar}";
    t "author" "@author \xce\xbb";
    t "param" "@param \xce\xbb";
    t "raise" "@raise \xce\xbb";
    t "see" "@see <\xce\xbb>";
    t "since" "@since \xce\xbb";
    t "before" "@before \xce\xbb";
    t "version" "@version \xce\xbb";
    t "right-brace" "\xce\xbb}";
  ];

  "comment-location", [
    t "error-on-first-line" "  @foo"
      ~location:{line = 2; column = 4};
    t "error-on-second-line" "  \n  @foo"
      ~location:{line = 2; column = 4};
  ];

  "unsupported", [
    (* test "index list"
      "{!indexlist}"
      (Ok []); *)
    t "left-alignment" "{L foo}";
    t "center-alignment" "{C foo}";
    t "right-alignment" "{R foo}";
    t "custom-style" "{c foo}";
    t "custom-tag" "@custom";
    (* test "custom reference kind"
      "{!custom:foo}"
      (Ok []); *)
    t "html-tag" "<b>foo</b>";
  ];

  (*
  "reference-target", [
    test "empty kind"
      "{!:foo}"
      (error 1 0 1 3 ["'{!...:' (reference kind) cannot be empty"]);

    test "with kind but empty"
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
        ["end of text is not allowed in '{!...}' (cross-reference)"]);
  ];
  *)
]



let expect_directory = "expect"
let actual_root_directory = "_actual"
let test_root = "test/parser"
let promote_script = "promote.sh"

let (//) = Filename.concat

(* We are temporarily using Lambda Soup, so take advantage of it. *)
let read_file = Soup.read_file
let write_file = Soup.write_file

let mkdir directory =
  try Unix.mkdir directory 0o755
  with Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> ()

let suggest_commands script_directory commands =
  let commands = String.concat "\n" commands in

  let script_file = script_directory // promote_script in

  if not (Sys.file_exists script_file) then
    write_file script_file commands;

  Printf.eprintf "\nbash %s\n\n"
    ("_build/default" // test_root // script_directory // promote_script)

let () =
  mkdir actual_root_directory;
  begin
    try Unix.unlink (actual_root_directory // promote_script);
    with _ -> ()
  end;

  let make_parser_test : string -> test_case -> unit Alcotest.test_case =
      fun suite case ->

    let run_test_case () =
      let file_title = case.name in
      let expect_suite_directory = expect_directory // suite in
      let actual_suite_directory = actual_root_directory // suite in
      let expect_file = expect_suite_directory // (file_title ^ ".txt") in
      let actual_file = actual_suite_directory // (file_title ^ ".txt") in

      let actual =
        let {sections_allowed; location; parser_input; _} = case in

        let dummy_filename = "f.ml" in

        let dummy_page =
          let root : Model.Root.t = {
            package = dummy_filename;
            file = Page dummy_filename;
            digest = String.make 16 '\000';
          }
          in
          Model.Paths.Identifier.Page (root, dummy_filename)
        in

        let location =
          {
            Lexing.pos_fname = dummy_filename;
            pos_lnum = location.line;
            pos_bol = 0;
            pos_cnum = location.column
          }
        in

        Parser_.parse_comment
          ~sections_allowed
          ~containing_definition:dummy_page
          ~location
          ~text:parser_input
        |> fun parser_output ->
          let buffer = Buffer.create 1024 in
          Print.parser_output (Format.formatter_of_buffer buffer) parser_output;
          Buffer.contents buffer
      in

      let expected =
        try read_file expect_file
        with Sys_error _ ->
          mkdir actual_suite_directory;
          write_file actual_file actual;

          prerr_newline ();
          prerr_endline case.parser_input;
          prerr_newline ();

          prerr_string actual;

          prerr_endline "\nThe expected output file does not exist.";
          prerr_endline "\nTo create it, run";
          [
            Printf.sprintf "mkdir -p %s"
              (test_root // expect_suite_directory);
            Printf.sprintf "cp %s %s"
              ("_build/default" // test_root // actual_file)
              (test_root // expect_file);
          ]
          |> suggest_commands actual_root_directory;

          Alcotest.fail "expected output file does not exist";
      in

      if actual <> expected then begin
        mkdir actual_suite_directory;
        write_file actual_file actual;

        prerr_newline ();
        prerr_endline case.parser_input;
        prerr_newline ();

        Printf.sprintf "diff -u %s %s" expect_file actual_file
        |> Sys.command
        |> ignore;

        prerr_endline "\nTo replace expected output with actual, run";
        [
          Printf.sprintf "cp %s %s"
            ("_build/default" // test_root // actual_file)
            (test_root // expect_file);
        ]
        |> suggest_commands actual_root_directory;

        Alcotest.fail "document tree incorrect";
      end
    in

    case.name, `Quick, run_test_case
  in

  let scan_for_stale_tests () =
    let directories = Sys.readdir expect_directory in
    directories |> Array.iter begin fun directory ->
      let source_directory = test_root // expect_directory // directory in

      let (_, tests) =
        try List.find (fun (suite_name, _) -> suite_name = directory) tests
        with Not_found ->
          Printf.eprintf
            "\n\nDirectory '%s' does not correspond to a test suite.\n"
            source_directory;
          prerr_endline "To remove it, run";
          suggest_commands actual_root_directory
            [Printf.sprintf "rm -r %s" source_directory];
          exit 1
      in

      let files = Sys.readdir (expect_directory // directory) in
      files |> Array.iter begin fun file ->
        let source_file = source_directory // file in
        let title = String.sub file 0 (String.length file - 4) in

        try List.find (fun {name; _} -> name = title) tests |> ignore
        with Not_found ->
          Printf.eprintf
            "\n\nExpect file '%s' does not correspond to a test.\n" source_file;
          prerr_endline "To remove it, run";
          suggest_commands actual_root_directory
            [Printf.sprintf "rm %s" source_file];
          exit 1
      end
    end
  in
  scan_for_stale_tests ();

  let tests : (unit Alcotest.test) list =
    tests |> List.map (fun (suite_name, test_cases) ->
      suite_name, List.map (make_parser_test suite_name) test_cases)
  in

  Alcotest.run "parser" tests
