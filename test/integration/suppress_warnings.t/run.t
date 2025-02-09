  $ ocamlc -c -bin-annot module_with_errors.mli
  $ ocamlc -c -bin-annot main.mli

  $ odoc compile module_with_errors.cmti
  $ odoc compile main.cmti -I .
  $ odoc link main.odoc
  File "module_with_errors.mli", line 7, characters 6-10:
  Warning: While resolving the expansion of include at File "main.mli", line 1, character 0
  Reference to 't' is ambiguous. Please specify its kind: section-t, type-t.
  $ odoc html-generate -o html main.odocl
  $ odoc support-files -o html

  $ odoc compile --warnings-tag foo module_with_errors.cmti
  $ odoc compile main.cmti -I .
  $ odoc link main.odoc

  $ odoc link --warnings-tags foo main.odoc
  File "module_with_errors.mli", line 7, characters 6-10:
  Warning: While resolving the expansion of include at File "main.mli", line 1, character 0
  Reference to 't' is ambiguous. Please specify its kind: section-t, type-t.

  $ odoc html-generate -o html2 main.odocl
  $ odoc support-files -o html2



