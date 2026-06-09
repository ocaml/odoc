open Odoc_parser_test

let groups =
  [
    ("trivial", Test.trivial);
    ("one_paragraph", Test.one_paragraph);
    ("two_paragraphs", Test.two_paragraphs);
    ("plus_minus_bar_words", Test.plus_minus_bar_words);
    ("escape_sequence", Test.escape_sequence);
    ("code_span", Test.code_span);
    ("bold", Test.bold);
    ("italic", Test.italic);
    ("emphasis", Test.emphasis);
    ("superscript", Test.superscript);
    ("subscript", Test.subscript);
    ("simple_reference", Test.simple_reference);
    ("reference_with_text", Test.reference_with_text);
    ("medias", Test.medias);
    ("link", Test.link);
    ("module_list", Test.module_list);
    ("code_block", Test.code_block);
    ("verbatim", Test.verbatim);
    ("shorthand_list", Test.shorthand_list);
    ("explicit_list", Test.explicit_list);
    ("deprecated", Test.deprecated);
    ("param", Test.param);
    ("raise", Test.raise);
    ("return", Test.return);
    ("see", Test.see);
    ("since", Test.since);
    ("before", Test.before);
    ("version", Test.version);
    ("canonical", Test.canonical);
    ("inline", Test.inline);
    ("open", Test.open_);
    ("closed", Test.closed);
    ("hidden", Test.hidden);
    ("bad_markup", Test.bad_markup);
    ("utf_8", Test.utf_8);
    ("comment_location", Test.comment_location);
    ("unsupported", Test.unsupported);
    ("locations", Test.locations);
    ("math", Test.math);
    ("heavy", Test_tables.heavy);
    ("light", Test_tables.light);
  ]

let () = Expect_test_helper.run groups
