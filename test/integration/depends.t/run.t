Testing the depends command.

  $ ocamlc -c -no-alias-deps -bin-annot -w -49 -o lib.cmti lib.mli
  $ ocamlc -c -bin-annot -I . -o lib_a.cmti a.mli
  $ ocamlc -c -bin-annot -I . -o lib_b.cmti b.mli

  $ odoc compile-deps lib_b.cmti | grep -v "CamlinternalFormatBasics\|Stdlib\|Pervasives" | cut -d ' ' -f 1 | sort -u
  Lib
  Lib_a
  Lib_b

  $ odoc compile --pkg lib -I . lib.cmti
  $ odoc compile --pkg lib -I . lib_a.cmti
  $ odoc compile --pkg lib -I . lib_b.cmti

  $ odoc link-deps . | cut -d ' ' -f 1-2 | sort
  lib Lib
  lib Lib_a
