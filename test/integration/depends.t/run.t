Testing the depends command.

  $ ocamlc -c -no-alias-deps -bin-annot -w -49 -o lib.cmti lib.mli
  $ ocamlc -c -bin-annot -I . -o lib_a.cmti a.mli
  $ ocamlc -c -bin-annot -I . -o lib_b.cmti b.mli

  $ odoc compile-deps lib_b.cmti | grep -v "CamlinternalFormatBasics\|Stdlib"
  Lib bbd67101d2e61e05dcf0f0cbf5f9dbe0
  Lib_a 21c81261177d685464037918ae900b81
  Lib_b b417b775de9c850fac708cc7fb6343b5

  $ odoc compile --pkg lib -I . lib.cmti
  $ odoc compile --pkg lib -I . lib_a.cmti
  $ odoc compile --pkg lib -I . lib_b.cmti

  $ odoc link-deps .
  lib Lib_a 21c81261177d685464037918ae900b81
  lib Lib bbd67101d2e61e05dcf0f0cbf5f9dbe0
