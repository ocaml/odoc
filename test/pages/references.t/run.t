  $ ocamlc -c -bin-annot Bar.mli
  $ ocamlc -c -bin-annot Baz.mli
  $ ocamlc -c -bin-annot foo.mli
  $ ocamlc -c -bin-annot moo.mli

  $ odoc compile page.mld --child Bar --child module-baz --child Foo --child module-Moo
  $ odoc compile Bar.cmti -I . --parent page
  $ odoc compile Baz.cmti -I . --parent page-page
  $ odoc compile foo.cmti -I . --parent page
  $ odoc compile moo.cmti -I . --parent page-page

  $ odoc link page-page.odoc -I .
  $ odoc link Bar.odoc -I .
  $ odoc link Baz.odoc -I .
  $ odoc link foo.odoc -I .
  $ odoc link moo.odoc -I .

  $ for i in *.odocl; do odoc html-generate $i -o html; done
  $ odoc support-files -o html
