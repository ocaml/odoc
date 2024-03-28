  $ ocamlc -c -bin-annot test.mli
  $ odoc compile test.cmti
  $ odoc link test.odoc
  $ odoc html-generate -o /tmp/html test.odocl
  $ odoc support-files -o /tmp/html

