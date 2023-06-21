  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  Indexing in 0.000130s
  index: internal error, uncaught exception:
         Invalid_argument("String.sub / Bytes.sub")
         
  [125]
  $ export SHERLODOC_DB=db.bin
  $ sherlodoc "unique_name"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc "multiple_hit"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc "name_conflict"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc "nesting_priority"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc --print-cost "list"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc --print-cost "map"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc --print-cost "list map"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc --print-cost ":moo"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc --print-cost ":moo -> _"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc --print-cost "modtype"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc --print-cost "S"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc --print-cost "qwertyuiopasdfghjklzxcvbnm"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
TODO : get a result for the query bellow
  $ sherlodoc --print-cost "hidden"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc --print-cost ":mo"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc ":'a"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc ": 'a -> 'b -> 'c "
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc ": ('a -> 'b) -> 'a t -> 'b t"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
TODO : get a result for the query bellow
  $ sherlodoc ": 'a bo"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
  $ sherlodoc ":extensible_type"
  sherlodoc: internal error, uncaught exception:
             End_of_file
             
  [125]
