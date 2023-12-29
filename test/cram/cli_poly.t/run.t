  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=ancient
  $ sherlodoc_index $(find . -name '*.odocl')
TODO : get a result for the query bellow
  $ sherlodoc ":'a"
  val Main.poly_1 : 'a -> 'b -> 'c
  $ sherlodoc ": 'a -> 'b -> 'c "
  val Main.poly_1 : 'a -> 'b -> 'c
TODO : get a result for the query bellow
