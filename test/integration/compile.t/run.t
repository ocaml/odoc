Various tests for the 'compile' command.

'ast.mli' is a collection of different syntaxes.

  $ ocamlc -bin-annot -c ast.mli
  $ odoc compile --package foo ast.cmti
  File "ast.mli", line 1, characters 4-17:
  Warning: Unknown tag '@TxtAttribute'.
  File "ast.mli", line 4, characters 4-21:
  Warning: Unknown tag '@ValueDeclaration'.
  File "ast.mli", line 6, characters 4-21:
  Warning: Unknown tag '@ValueDeclaration'.
  File "ast.mli", line 8, characters 4-21:
  Warning: Unknown tag '@ValueDeclaration'.
  File "ast.mli", line 11, characters 4-20:
  Warning: Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 13, characters 4-20:
  Warning: Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 14, characters 16-39:
  Warning: Unknown tag '@ConstructorDeclaration'.
  File "ast.mli", line 15, characters 16-39:
  Warning: Unknown tag '@ConstructorDeclaration'.
  File "ast.mli", line 22, characters 4-20:
  Warning: Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 24, characters 15-32:
  Warning: Unknown tag '@LabelDeclaration'.
  File "ast.mli", line 25, characters 16-33:
  Warning: Unknown tag '@LabelDeclaration'.
  File "ast.mli", line 29, characters 4-20:
  Warning: Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 31, characters 4-18:
  Warning: Unknown tag '@TypeExtension'.
  File "ast.mli", line 32, characters 17-27:
  Warning: Unknown tag '@Extension'.
  File "ast.mli", line 33, characters 17-27:
  Warning: Unknown tag '@Extension'.
  File "ast.mli", line 35, characters 4-26:
  Warning: Unknown tag '@ModuleTypeDeclaration'.
  File "ast.mli", line 37, characters 6-19:
  Warning: Unknown tag '@TxtAttribute'.
  File "ast.mli", line 40, characters 6-22:
  Warning: Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 43, characters 4-22:
  Warning: Unknown tag '@ModuleDeclaration'.
  File "ast.mli", line 45, characters 6-19:
  Warning: Unknown tag '@TxtAttribute'.
  File "ast.mli", line 47, characters 6-24:
  Warning: Unknown tag '@ModuleDeclaration'.
  File "ast.mli", line 49, characters 8-21:
  Warning: Unknown tag '@TxtAttribute'.
  File "ast.mli", line 52, characters 8-24:
  Warning: Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 57, characters 4-14:
  Warning: Unknown tag '@Exception'.
  File "ast.mli", line 60, characters 4-11:
  Warning: Unknown tag '@Hidden'.
  File "ast.mli", line 63, characters 4-23:
  Warning: Unknown tag '@IncludeDescription'.
  File "ast.mli", line 68, characters 6-22:
  Warning: Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 65, characters 6-19:
  Warning: Unknown tag '@TxtAttribute'.
  File "ast.mli", line 68, characters 6-22:
  Warning: Unknown tag '@TypeDeclaration'.
  File "ast.mli", line 71, characters 4-10:
  Warning: Unknown tag '@Class'.
  File "ast.mli", line 74, characters 6-13:
  Warning: Unknown tag '@Method'.

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
  File "parser_errors.mli", line 43, characters 4-15:
  Warning: Unknown tag '@UnknownTag'.
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
  File "parser_errors.mli", line 43, characters 4-15:
  Error: Unknown tag '@UnknownTag'.
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
