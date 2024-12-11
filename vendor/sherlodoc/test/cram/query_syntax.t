We need a dummy file because sherlodoc requires an odocl.
  $ touch main.mli
  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ export SHERLODOC_FORMAT=marshal
  $ export SHERLODOC_DB=db.bin
  $ sherlodoc index main.odocl
  $ sherlodoc search --pretty-query ": int list option"
   : int list option
  [No results]
  $ export OCAMLRUNPARAM=b
  $ sherlodoc search --pretty-query ": _"
   : _
  [No results]
Testing incomplete queries
  $ sherlodoc search --pretty-query ": ->"
   : _ -> _
  [No results]
  $ sherlodoc search --pretty-query ": int ->"
   : int -> _
  [No results]
  $ sherlodoc search --pretty-query ": int *"
   : int * _
  [No results]
  $ sherlodoc search --pretty-query ": string -> ("
   : string -> _
  [No results]
  $ sherlodoc search --pretty-query ": (int"
   : int
  [No results]
  $ sherlodoc search --pretty-query ": (int ->"
   : int -> _
  [No results]
  $ sherlodoc search --pretty-query ": (int *"
   : int * _
  [No results]
  $ sherlodoc search --pretty-query ": foo bar qux"
   : foo bar qux
  [No results]
  $ sherlodoc search --pretty-query ": ()"
   : _
  [No results]
  $ sherlodoc search --pretty-query ": )"
   : _
  [No results]
  $ sherlodoc search --pretty-query ": (int,"
   : int * _
  [No results]
  $ sherlodoc search --pretty-query ": (int,string"
   : int * string
  [No results]
  $ sherlodoc search --pretty-query ": 'a, 'b) result -"
   : ('a, 'b) result -> _
  [No results]
  $ sherlodoc search --pretty-query ": 'a * 'b) list"
   : ('a * 'b) list
  [No results]
  $ sherlodoc search --pretty-query ": - ,'a * 'b, 'c) result -) - ( -"
   : ((_ -> _, 'a * 'b, 'c) result -> _) -> _ -> _
  [No results]
Testing syntax errors
  $ sherlodoc search --pretty-query ": )("
   : <parsing error>
  [No results]
