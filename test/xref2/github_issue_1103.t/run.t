  $ ocamlc -bin-annot -c to_open.mli
  $ ocamlc -bin-annot -c test.mli
  $ odoc compile to_open.cmti
  $ odoc compile -I . test.cmti

The following should not result in any unresolved references

  $ odoc link -I . --open To_open test.odoc
  $ odoc link -I . to_open.odoc


