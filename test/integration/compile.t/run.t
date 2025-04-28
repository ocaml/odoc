Various tests for the 'compile' command.

'ast.mli' is a collection of different syntaxes.

  $ ocamlc -bin-annot -c ast.mli
  $ odoc compile --package foo ast.cmti

Test different parsing errors.

  $ ocamlc -bin-annot -c parser_errors.mli
  $ odoc compile --package foo parser_errors.cmti
  File "parser_errors.mli", line 1, characters 4-26:
  Warning: '{x This is bad markup}': bad markup.
  Suggestion: did you mean '{!x This is bad markup}' or '[x This is bad markup]'?
  File "parser_errors.mli", line 4, characters 4-24:
  Warning: '9': bad heading level (0-5 allowed).
  File "parser_errors.mli", line 10, characters 8-11:
  Warning: '{li ...}' should be followed by space, a tab, or a new line.
  File "parser_errors.mli", line 13, characters 4-7:
  Warning: '{li ...}' (list item) is not allowed in top-level text.
  Suggestion: move '{li ...}' into '{ul ...}' (bulleted list), or use '-' (bulleted list item).
  File "parser_errors.mli", line 13, characters 19-20:
  Warning: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 16, characters 4-6:
  Warning: '{v' should be followed by whitespace.
  File "parser_errors.mli", line 19, characters 37-39:
  Warning: 'v}' should be preceded by whitespace.
  File "parser_errors.mli", line 22, characters 4-5:
  Warning: Stray '@'.
  File "parser_errors.mli", line 28, characters 4-11:
  Warning: '@before' expects version number on the same line.
  File "parser_errors.mli", line 31, characters 4-10:
  Warning: '@param' expects parameter name on the same line.
  File "parser_errors.mli", line 34, characters 4-10:
  Warning: '@raise' expects exception constructor on the same line.
  File "parser_errors.mli", line 34, characters 4-4:
  Warning: Identifier in reference should not be empty.
  File "parser_errors.mli", line 37, characters 4-11:
  Warning: '@raises' expects exception constructor on the same line.
  File "parser_errors.mli", line 37, characters 4-4:
  Warning: Identifier in reference should not be empty.
  File "parser_errors.mli", line 40, characters 4-8:
  Warning: '@see' should be followed by <url>, 'file', or "document title".
  File "parser_errors.mli", line 46, characters 4-5:
  Warning: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 49, characters 4-5:
  Warning: Unpaired ']' (end of code).
  Suggestion: try '\]'.
  File "parser_errors.mli", line 53, characters 4-5:
  Warning: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 56, characters 4-18:
  Warning: '{x bad markup}': bad markup.
  Suggestion: did you mean '{!x bad markup}' or '[x bad markup]'?

With warn-error enabled.

  $ odoc compile --package foo --warn-error parser_errors.cmti
  File "parser_errors.mli", line 1, characters 4-26:
  Error: '{x This is bad markup}': bad markup.
  Suggestion: did you mean '{!x This is bad markup}' or '[x This is bad markup]'?
  File "parser_errors.mli", line 4, characters 4-24:
  Error: '9': bad heading level (0-5 allowed).
  File "parser_errors.mli", line 10, characters 8-11:
  Error: '{li ...}' should be followed by space, a tab, or a new line.
  File "parser_errors.mli", line 13, characters 4-7:
  Error: '{li ...}' (list item) is not allowed in top-level text.
  Suggestion: move '{li ...}' into '{ul ...}' (bulleted list), or use '-' (bulleted list item).
  File "parser_errors.mli", line 13, characters 19-20:
  Error: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 16, characters 4-6:
  Error: '{v' should be followed by whitespace.
  File "parser_errors.mli", line 19, characters 37-39:
  Error: 'v}' should be preceded by whitespace.
  File "parser_errors.mli", line 22, characters 4-5:
  Error: Stray '@'.
  File "parser_errors.mli", line 28, characters 4-11:
  Error: '@before' expects version number on the same line.
  File "parser_errors.mli", line 31, characters 4-10:
  Error: '@param' expects parameter name on the same line.
  File "parser_errors.mli", line 34, characters 4-10:
  Error: '@raise' expects exception constructor on the same line.
  File "parser_errors.mli", line 34, characters 4-4:
  Error: Identifier in reference should not be empty.
  File "parser_errors.mli", line 37, characters 4-11:
  Error: '@raises' expects exception constructor on the same line.
  File "parser_errors.mli", line 37, characters 4-4:
  Error: Identifier in reference should not be empty.
  File "parser_errors.mli", line 40, characters 4-8:
  Error: '@see' should be followed by <url>, 'file', or "document title".
  File "parser_errors.mli", line 46, characters 4-5:
  Error: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 49, characters 4-5:
  Error: Unpaired ']' (end of code).
  Suggestion: try '\]'.
  File "parser_errors.mli", line 53, characters 4-5:
  Error: Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 56, characters 4-18:
  Error: '{x bad markup}': bad markup.
  Suggestion: did you mean '{!x bad markup}' or '[x bad markup]'?
  ERROR: Warnings have been generated.
  [1]

Compiling a '.cmt' file.

  $ ocamlc -bin-annot -c impl_only.ml
  $ odoc compile --package foo impl_only.cmt

Check line numbers for errors in a '.mld' file.

  $ odoc compile line_numbers.mld
  File "line_numbers.mld", line 2, characters 0-4:
  Warning: '{[...]}' (code block) should not be empty.
  File "line_numbers.mld", line 8, characters 0-12:
  Warning: '{Bad Markup}': bad markup.
  Suggestion: did you mean '{!Bad Markup}' or '[Bad Markup]'?
  File "line_numbers.mld", line 32, characters 0-1:
  Warning: '{': bad markup.
  Suggestion: escape the brace with '\{'.
  File "line_numbers.mld", line 16, characters 0-11:
  Warning: '6': bad heading level (0-5 allowed).
