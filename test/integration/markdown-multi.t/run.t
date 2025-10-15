Test markdown-generate with multiple .odocl files in a single invocation:

  $ ocamlc -c -bin-annot test.mli
  $ ocamlc -c -bin-annot test2.mli
  $ ocamlc -c -bin-annot list.mli
  $ odoc compile --package test -I . page.mld
  File "page.mld", line 25, characters 23-34:
  Warning: '{{!...} ...}' (cross-reference) should not be empty.
  File "page.mld", line 35, characters 23-32:
  Warning: '{{!...} ...}' (cross-reference) should not be empty.
  File "page.mld", line 35, characters 49-59:
  Warning: '{{!...} ...}' (cross-reference) should not be empty.
  File "page.mld", line 125, characters 0-11:
  Warning: Tags are not allowed in pages.
  $ odoc compile --package test test.cmti
  $ odoc compile --package test -I . test2.cmti
  $ odoc compile --package list -I . list.cmti
  File "list.mli", line 1, characters 4-12:
  Warning: '{0': heading level should be lower than top heading level '0'.
  $ odoc link test.odoc
  $ odoc link test2.odoc
  $ odoc link list.odoc
  File "list.mli", line 37, characters 12-19:
  Warning: Reference to 'head' is ambiguous. Please specify its kind: section-head, val-head.
  $ odoc link page-page.odoc
  File "page.mld", line 85, characters 0-33:
  Warning: Failed to resolve reference ./odoc_logo_placeholder.jpg Path 'odoc_logo_placeholder.jpg' not found
  File "page.mld", line 29, characters 4-49:
  Warning: Failed to resolve reference ./test.mli Path 'test' not found

Generate with separate invocations:
  $ odoc markdown-generate test.odocl -o markdown-separate
  $ odoc markdown-generate test2.odocl -o markdown-separate
  $ odoc markdown-generate page-page.odocl -o markdown-separate
  $ odoc markdown-generate list.odocl -o markdown-separate

Generate with multiple files in one invocation:
  $ odoc markdown-generate test.odocl test2.odocl page-page.odocl list.odocl -o markdown-multi

Verify both approaches produce identical output:
  $ diff -r markdown-separate markdown-multi

