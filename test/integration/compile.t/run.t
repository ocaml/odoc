Various tests for the 'compile' command.

'ast.mli' is a collection of different syntaxes.

  $ ocamlc -bin-annot -c ast.mli
  $ odoc compile --package foo ast.cmti
  File "ast.mli", line 1, characters 4-17:
  Unknown tag '@TxtAttribute'.
  File "ast.mli", line 4, characters 4-21:
  Unknown tag '@ValueDeclaration'.
  File "ast.mli", line 6, characters 4-21:
  Unknown tag '@ValueDeclaration'.
  File "ast.mli", line 8, characters 4-21:
  Unknown tag '@ValueDeclaration'.
  File "ast.mli", line 11, characters 4-20:
  Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 13, characters 4-20:
  Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 14, characters 16-39:
  Unknown tag '@ConstructorDeclaration'.
  File "ast.mli", line 15, characters 16-39:
  Unknown tag '@ConstructorDeclaration'.
  File "ast.mli", line 22, characters 4-20:
  Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 24, characters 15-32:
  Unknown tag '@LabelDeclaration'.
  File "ast.mli", line 25, characters 16-33:
  Unknown tag '@LabelDeclaration'.
  File "ast.mli", line 29, characters 4-20:
  Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 31, characters 4-18:
  Unknown tag '@TypeExtension'.
  File "ast.mli", line 32, characters 17-27:
  Unknown tag '@Extension'.
  File "ast.mli", line 33, characters 17-27:
  Unknown tag '@Extension'.
  File "ast.mli", line 35, characters 4-26:
  Unknown tag '@ModuleTypeDeclaration'.
  File "ast.mli", line 37, characters 6-19:
  Unknown tag '@TxtAttribute'.
  File "ast.mli", line 40, characters 6-22:
  Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 43, characters 4-22:
  Unknown tag '@ModuleDeclaration'.
  File "ast.mli", line 45, characters 6-19:
  Unknown tag '@TxtAttribute'.
  File "ast.mli", line 47, characters 6-24:
  Unknown tag '@ModuleDeclaration'.
  File "ast.mli", line 49, characters 8-21:
  Unknown tag '@TxtAttribute'.
  File "ast.mli", line 52, characters 8-24:
  Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 57, characters 4-14:
  Unknown tag '@Exception'.
  File "ast.mli", line 60, characters 4-11:
  Unknown tag '@Hidden'.
  File "ast.mli", line 63, characters 4-23:
  Unknown tag '@IncludeDescription'.
  File "ast.mli", line 68, characters 6-22:
  Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 65, characters 6-19:
  Unknown tag '@TxtAttribute'.
  File "ast.mli", line 68, characters 6-22:
  Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 71, characters 4-10:
  Unknown tag '@Class'.
  File "ast.mli", line 74, characters 6-13:
  Unknown tag '@Method'.

Test different parsing errors.

  $ ocamlc -bin-annot -c parser_errors.mli
  $ odoc compile --package foo parser_errors.cmti
  File "parser_errors.mli", line 1, characters 4-26:
  '{x This is bad markup}': bad markup.
  Suggestion: did you mean '{!x This is bad markup}' or '[x This is bad markup]'?
  File "parser_errors.mli", line 4, characters 4-24:
  '9': bad heading level (0-5 allowed).
  File "parser_errors.mli", line 10, characters 8-11:
  '{li ...}' should be followed by space, a tab, or a new line.
  File "parser_errors.mli", line 13, characters 4-7:
  '{li ...}' (list item) is not allowed in top-level text.
  Suggestion: move '{li ...}' into '{ul ...}' (bulleted list), or use '-' (bulleted list item).
  File "parser_errors.mli", line 13, characters 19-20:
  Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 16, characters 4-6:
  '{v' should be followed by whitespace.
  File "parser_errors.mli", line 19, characters 37-39:
  'v}' should be preceded by whitespace.
  File "parser_errors.mli", line 22, characters 4-5:
  Stray '@'.
  File "parser_errors.mli", line 28, characters 4-11:
  '@before' expects version number on the same line.
  File "parser_errors.mli", line 31, characters 4-10:
  '@param' expects parameter name on the same line.
  File "parser_errors.mli", line 34, characters 4-10:
  '@raise' expects exception constructor on the same line.
  File "parser_errors.mli", line 37, characters 4-11:
  '@raises' expects exception constructor on the same line.
  File "parser_errors.mli", line 40, characters 4-8:
  '@see' should be followed by <url>, 'file', or "document title".
  File "parser_errors.mli", line 43, characters 4-15:
  Unknown tag '@UnknownTag'.
  File "parser_errors.mli", line 46, characters 4-5:
  Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 49, characters 4-5:
  Unpaired ']' (end of code).
  Suggestion: try '\]'.
  File "parser_errors.mli", line 53, characters 4-5:
  Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 56, characters 4-18:
  '{x bad markup}': bad markup.
  Suggestion: did you mean '{!x bad markup}' or '[x bad markup]'?

With warn-error enabled.

  $ odoc compile --package foo --warn-error parser_errors.cmti
  File "parser_errors.mli", line 1, characters 4-26:
  '{x This is bad markup}': bad markup.
  Suggestion: did you mean '{!x This is bad markup}' or '[x This is bad markup]'?
  File "parser_errors.mli", line 4, characters 4-24:
  '9': bad heading level (0-5 allowed).
  File "parser_errors.mli", line 10, characters 8-11:
  '{li ...}' should be followed by space, a tab, or a new line.
  File "parser_errors.mli", line 13, characters 4-7:
  '{li ...}' (list item) is not allowed in top-level text.
  Suggestion: move '{li ...}' into '{ul ...}' (bulleted list), or use '-' (bulleted list item).
  File "parser_errors.mli", line 13, characters 19-20:
  Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 16, characters 4-6:
  '{v' should be followed by whitespace.
  File "parser_errors.mli", line 19, characters 37-39:
  'v}' should be preceded by whitespace.
  File "parser_errors.mli", line 22, characters 4-5:
  Stray '@'.
  File "parser_errors.mli", line 28, characters 4-11:
  '@before' expects version number on the same line.
  File "parser_errors.mli", line 31, characters 4-10:
  '@param' expects parameter name on the same line.
  File "parser_errors.mli", line 34, characters 4-10:
  '@raise' expects exception constructor on the same line.
  File "parser_errors.mli", line 37, characters 4-11:
  '@raises' expects exception constructor on the same line.
  File "parser_errors.mli", line 40, characters 4-8:
  '@see' should be followed by <url>, 'file', or "document title".
  File "parser_errors.mli", line 43, characters 4-15:
  Unknown tag '@UnknownTag'.
  File "parser_errors.mli", line 46, characters 4-5:
  Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 49, characters 4-5:
  Unpaired ']' (end of code).
  Suggestion: try '\]'.
  File "parser_errors.mli", line 53, characters 4-5:
  Unpaired '}' (end of markup).
  Suggestion: try '\}'.
  File "parser_errors.mli", line 56, characters 4-18:
  '{x bad markup}': bad markup.
  Suggestion: did you mean '{!x bad markup}' or '[x bad markup]'?
  ERROR: Warnings have been generated.
  [1]

Compiling a '.cmt' file.

  $ ocamlc -bin-annot -c impl_only.ml
  $ odoc compile --package foo impl_only.cmt

Check line numbers for errors in a '.mld' file.

  $ odoc compile line_numbers.mld
  File "line_numbers.mld", line 2, characters 0-4:
  '{[...]}' (code block) should not be empty.
  File "line_numbers.mld", line 8, characters 0-12:
  '{Bad Markup}': bad markup.
  Suggestion: did you mean '{!Bad Markup}' or '[Bad Markup]'?
  File "line_numbers.mld", line 32, characters 0-1:
  '{': bad markup.
  Suggestion: escape the brace with '\{'.
  File "line_numbers.mld", line 16, characters 0-11:
  '6': bad heading level (0-5 allowed).
