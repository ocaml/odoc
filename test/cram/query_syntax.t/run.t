  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  $ export SHERLODOC_DB=db.bin
  $ sherlodoc --pretty-query ": int list option"
   : int list option
  [No results]
  $ sherlodoc --pretty-query ": ->"
   : _
  [No results]
  $ sherlodoc --pretty-query ": int ->"
   : int -> _
  [No results]
  $ sherlodoc --pretty-query ": int *"
   : int * _
  [No results]
  $ sherlodoc --pretty-query ": string -> ("
   : string -> _
  [No results]
  $ sherlodoc --pretty-query ": (int"
   : _
  [No results]
