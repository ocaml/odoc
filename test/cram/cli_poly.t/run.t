  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  Index_lib.main
  Indexing in 0.000579s
  trie_with_array_occ:0.00s
  trie_with_array:0.00s
  Cache.Elt_array_occ_trie.memo:0.00s
  Cache.Elt_array_trie.memo:0.00s
  $ export SHERLODOC_DB=db.bin
TODO : get a result for the query bellow
  $ sherlodoc ":'a"
  val Main.poly_1 : 'a -> 'b -> 'c
  $ sherlodoc ": 'a -> 'b -> 'c "
  val Main.poly_1 : 'a -> 'b -> 'c
TODO : get a result for the query bellow
