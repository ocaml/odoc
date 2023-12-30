We need a dummy file because sherlodoc requires an odocl.
  $ touch main.mli
  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ export SHERLODOC_FORMAT=marshal
  $ export SHERLODOC_DB=db.bin
  $ sherlodoc_index main.odocl
  $ sherlodoc --pretty-query ": int list option"
   : int list option
  [No results]
  $ export OCAMLRUNPARAM=b
  $ sherlodoc --pretty-query ": _"
   : _
  [No results]
Testing incomplete queries
  $ sherlodoc --pretty-query ": ->"
   : _ -> _
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
   : int
  [No results]
  $ sherlodoc --pretty-query ": (int ->"
   : int -> _
  [No results]
  $ sherlodoc --pretty-query ": (int *"
   : int * _
  [No results]
  $ sherlodoc --pretty-query ": foo bar qux"
   : foo bar qux
  [No results]
  $ sherlodoc --pretty-query ": ()"
   : _
  [No results]
  $ sherlodoc --pretty-query ": )"
   : _
  [No results]
  $ sherlodoc --pretty-query ": (int,"
   : int * _
  [No results]
  $ sherlodoc --pretty-query ": (int,string"
   : int * string
  [No results]
  $ sherlodoc --pretty-query ": 'a, 'b) result -"
   : ('a, 'b) result -> _
  [No results]
  $ sherlodoc --pretty-query ": 'a * 'b) list"
   : ('a * 'b) list
  [No results]
  $ sherlodoc --pretty-query ": - ,'a * 'b, 'c) result -) - ( -"
   : ((_ -> _, 'a * 'b, 'c) result -> _) -> _ -> _
  [No results]
Testing syntax errors
  $ sherlodoc --pretty-query ": )("
   : <parsing error>
  [No results]
