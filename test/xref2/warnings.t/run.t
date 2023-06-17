Test the behavior of warnings generated while compiling and linking.

  $ ocamlc -c -bin-annot b.mli
  $ ocamlc -c -bin-annot a.mli

A contains both parsing errors and a reference to B that isn't compiled yet:

  $ odoc compile --warn-error --package test a.cmti
  File "a.mli", line 8, characters 20-23:
  Error: Open bracket '{!' is never closed.
  File "a.mli", line 8, characters 22-23:
  Error: Identifier in reference should not be empty.
  ERROR: Warnings have been generated.
  [1]

  $ odoc compile --package test b.cmti
  $ odoc compile --package test a.cmti
  File "a.mli", line 8, characters 20-23:
  Warning: Open bracket '{!' is never closed.
  File "a.mli", line 8, characters 22-23:
  Warning: Identifier in reference should not be empty.

  $ odoc errors a.odoc
  File "a.mli", line 8, characters 20-23:
  Open bracket '{!' is never closed.
  File "a.mli", line 8, characters 22-23:
  Identifier in reference should not be empty.

A contains linking errors:

  $ odoc link a.odoc --enable-missing-root-warning
  File "a.odoc":
  Warning: Couldn't find the following modules:
    B
  File "a.mli", line 6, characters 47-65:
  Warning: Failed to resolve reference unresolvedroot(B).doesn't_exist Couldn't find "B"

  $ odoc errors a.odocl
  File "a.mli", line 8, characters 20-23:
  Open bracket '{!' is never closed.
  File "a.mli", line 8, characters 22-23:
  Identifier in reference should not be empty.
  File "a.odoc":
  Couldn't find the following modules:
    B
  File "a.mli", line 6, characters 47-65:
  Failed to resolve reference unresolvedroot(B).doesn't_exist Couldn't find "B"

It is possible to hide the warnings too:

  $ odoc compile --print-warnings false --package test a.cmti
  $ odoc link --print-warnings false a.odoc
  $ ODOC_PRINT_WARNINGS=false odoc compile --package test a.cmti
