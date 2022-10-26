Testing the depends command.

  $ ocamlc -c -no-alias-deps -bin-annot -w -49 -o lib.cmti lib.mli
  $ ocamlc -c -bin-annot -I . -o lib_a.cmti a.mli
  $ ocamlc -c -bin-annot -I . -o lib_b.cmti b.mli

  $ odoc compile-deps lib_b.cmti | grep -v "CamlinternalFormatBasics\|Stdlib\|Pervasives" | cut -d ' ' -f 1 | sort -u
  Lib
  Lib_a
  Lib_b

  $ odoc compile --pkg lib -I . lib.cmti
  File "lib.cmti":
  Warning: No implementation file found for the given interface
  $ odoc compile --pkg lib -I . lib_a.cmti
  File "lib_a.cmti":
  Warning: No implementation file found for the given interface
  $ odoc compile --pkg lib -I . lib_b.cmti
  File "lib_b.cmti":
  Warning: No implementation file found for the given interface

  $ odoc link-deps . | cut -d ' ' -f 1-2 | sort
  lib Lib
  lib Lib_a
