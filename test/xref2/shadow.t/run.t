This test contains a carefully crafted couple of modules that cause a clash of names in the current shadowing code.
If the shadowing code is working correctly this should not crash

  $ ocamlc -c -bin-annot -I . c.mli
  $ ocamlc -c -bin-annot -I . d.mli
  $ odoc compile -I . c.cmti
  $ odoc compile -I . d.cmti

