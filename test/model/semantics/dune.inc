(rule
 (alias runtest)
 (package odoc)
 (action
  (progn
   (with-stdout-to
    simple_reference.actual
    (run %{exe:test.exe} simple_reference))
   (diff expected/simple_reference.expected simple_reference.actual))))

(rule
 (alias runtest)
 (package odoc)
 (action
  (progn
   (with-stdout-to
    raw_markup.actual
    (run %{exe:test.exe} raw_markup))
   (diff expected/raw_markup.expected raw_markup.actual))))

(rule
 (alias runtest)
 (package odoc)
 (action
  (progn
   (with-stdout-to
    section_contexts.actual
    (run %{exe:test.exe} section_contexts))
   (diff expected/section_contexts.expected section_contexts.actual))))

(rule
 (alias runtest)
 (package odoc)
 (action
  (progn
   (with-stdout-to
    heading.actual
    (run %{exe:test.exe} heading))
   (diff expected/heading.expected heading.actual))))

(rule
 (alias runtest)
 (package odoc)
 (action
  (progn
   (with-stdout-to
    author.actual
    (run %{exe:test.exe} author))
   (diff expected/author.expected author.actual))))

(rule
 (alias runtest)
 (package odoc)
 (action
  (progn
   (with-stdout-to
    reference_component_kind.actual
    (run %{exe:test.exe} reference_component_kind))
   (diff
    expected/reference_component_kind.expected
    reference_component_kind.actual))))

(rule
 (alias runtest)
 (package odoc)
 (action
  (progn
   (with-stdout-to
    reference_path.actual
    (run %{exe:test.exe} reference_path))
   (diff expected/reference_path.expected reference_path.actual))))
