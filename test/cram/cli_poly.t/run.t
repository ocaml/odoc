  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  Indexing in 0.602961ms
  Export in 0.049114ms
  $ export SHERLODOC_DB=db.bin
TODO : get a result for the query bellow
  $ sherlodoc ":'a"
  val Main.poly_1 : 'a -> 'b -> 'c
  $ sherlodoc ": 'a -> 'b -> 'c "
  val Main.poly_1 : 'a -> 'b -> 'c
TODO : get a result for the query bellow
