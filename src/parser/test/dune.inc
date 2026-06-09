(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    trivial.actual
    (run %{exe:odoc_parser_test_main.exe} trivial))
   (diff expected/trivial.expected trivial.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    one_paragraph.actual
    (run %{exe:odoc_parser_test_main.exe} one_paragraph))
   (diff expected/one_paragraph.expected one_paragraph.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    two_paragraphs.actual
    (run %{exe:odoc_parser_test_main.exe} two_paragraphs))
   (diff expected/two_paragraphs.expected two_paragraphs.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    plus_minus_bar_words.actual
    (run %{exe:odoc_parser_test_main.exe} plus_minus_bar_words))
   (diff expected/plus_minus_bar_words.expected plus_minus_bar_words.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    escape_sequence.actual
    (run %{exe:odoc_parser_test_main.exe} escape_sequence))
   (diff expected/escape_sequence.expected escape_sequence.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    code_span.actual
    (run %{exe:odoc_parser_test_main.exe} code_span))
   (diff expected/code_span.expected code_span.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    bold.actual
    (run %{exe:odoc_parser_test_main.exe} bold))
   (diff expected/bold.expected bold.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    italic.actual
    (run %{exe:odoc_parser_test_main.exe} italic))
   (diff expected/italic.expected italic.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    emphasis.actual
    (run %{exe:odoc_parser_test_main.exe} emphasis))
   (diff expected/emphasis.expected emphasis.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    superscript.actual
    (run %{exe:odoc_parser_test_main.exe} superscript))
   (diff expected/superscript.expected superscript.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    subscript.actual
    (run %{exe:odoc_parser_test_main.exe} subscript))
   (diff expected/subscript.expected subscript.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    simple_reference.actual
    (run %{exe:odoc_parser_test_main.exe} simple_reference))
   (diff expected/simple_reference.expected simple_reference.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    reference_with_text.actual
    (run %{exe:odoc_parser_test_main.exe} reference_with_text))
   (diff expected/reference_with_text.expected reference_with_text.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    medias.actual
    (run %{exe:odoc_parser_test_main.exe} medias))
   (diff expected/medias.expected medias.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    link.actual
    (run %{exe:odoc_parser_test_main.exe} link))
   (diff expected/link.expected link.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    module_list.actual
    (run %{exe:odoc_parser_test_main.exe} module_list))
   (diff expected/module_list.expected module_list.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    code_block.actual
    (run %{exe:odoc_parser_test_main.exe} code_block))
   (diff expected/code_block.expected code_block.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    verbatim.actual
    (run %{exe:odoc_parser_test_main.exe} verbatim))
   (diff expected/verbatim.expected verbatim.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    shorthand_list.actual
    (run %{exe:odoc_parser_test_main.exe} shorthand_list))
   (diff expected/shorthand_list.expected shorthand_list.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    explicit_list.actual
    (run %{exe:odoc_parser_test_main.exe} explicit_list))
   (diff expected/explicit_list.expected explicit_list.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    deprecated.actual
    (run %{exe:odoc_parser_test_main.exe} deprecated))
   (diff expected/deprecated.expected deprecated.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    param.actual
    (run %{exe:odoc_parser_test_main.exe} param))
   (diff expected/param.expected param.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    raise.actual
    (run %{exe:odoc_parser_test_main.exe} raise))
   (diff expected/raise.expected raise.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    return.actual
    (run %{exe:odoc_parser_test_main.exe} return))
   (diff expected/return.expected return.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    see.actual
    (run %{exe:odoc_parser_test_main.exe} see))
   (diff expected/see.expected see.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    since.actual
    (run %{exe:odoc_parser_test_main.exe} since))
   (diff expected/since.expected since.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    before.actual
    (run %{exe:odoc_parser_test_main.exe} before))
   (diff expected/before.expected before.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    version.actual
    (run %{exe:odoc_parser_test_main.exe} version))
   (diff expected/version.expected version.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    canonical.actual
    (run %{exe:odoc_parser_test_main.exe} canonical))
   (diff expected/canonical.expected canonical.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    inline.actual
    (run %{exe:odoc_parser_test_main.exe} inline))
   (diff expected/inline.expected inline.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    open.actual
    (run %{exe:odoc_parser_test_main.exe} open))
   (diff expected/open.expected open.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    closed.actual
    (run %{exe:odoc_parser_test_main.exe} closed))
   (diff expected/closed.expected closed.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    hidden.actual
    (run %{exe:odoc_parser_test_main.exe} hidden))
   (diff expected/hidden.expected hidden.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    bad_markup.actual
    (run %{exe:odoc_parser_test_main.exe} bad_markup))
   (diff expected/bad_markup.expected bad_markup.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    utf_8.actual
    (run %{exe:odoc_parser_test_main.exe} utf_8))
   (diff expected/utf_8.expected utf_8.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    comment_location.actual
    (run %{exe:odoc_parser_test_main.exe} comment_location))
   (diff expected/comment_location.expected comment_location.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    unsupported.actual
    (run %{exe:odoc_parser_test_main.exe} unsupported))
   (diff expected/unsupported.expected unsupported.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    locations.actual
    (run %{exe:odoc_parser_test_main.exe} locations))
   (diff expected/locations.expected locations.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    math.actual
    (run %{exe:odoc_parser_test_main.exe} math))
   (diff expected/math.expected math.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    heavy.actual
    (run %{exe:odoc_parser_test_main.exe} heavy))
   (diff expected/heavy.expected heavy.actual))))

(rule
 (alias runtest)
 (action
  (progn
   (with-stdout-to
    light.actual
    (run %{exe:odoc_parser_test_main.exe} light))
   (diff expected/light.expected light.actual))))
