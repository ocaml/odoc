  $ ocamlc -c -bin-annot test.mli
  $ ocamlc -c -bin-annot test2.mli
  $ printf "{0 The title}\n" > page.mld
  $ odoc compile --package test test.cmti
  $ odoc compile --package test -I . test2.cmti
  $ odoc compile --package test -I . page.mld
  $ odoc link test.odoc
  $ odoc link test2.odoc
  $ odoc link page-page.odoc
  $ odoc markdown-generate test.odocl -o markdown
  $ ls markdown
