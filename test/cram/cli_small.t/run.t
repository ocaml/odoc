  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  Indexing in 0.222206ms
  Export in 0.052929ms
  $ export SHERLODOC_DB=db.bin
  $ sherlodoc --print-cost "list"
  36 mod Main.List
  136 type Main.list
  144 type Main.List.t = 'a list
  222 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
