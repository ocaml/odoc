Compile the modules:

  $ ocamlc -c a.mli a.ml -bin-annot

Compile the pages:

  $ odoc compile a.cmti
  Loc of module M: File "a.ml", line 1, characters 0-21
  Loc of module N: File "a.ml", lines 3-7, characters 0-3
  Loc of module S: File "a.ml", lines 4-6, characters 2-5
