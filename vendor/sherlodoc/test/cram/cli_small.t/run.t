  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index $(find . -name '*.odocl')
  $ sherlodoc search --print-cost "list"
  139 type 'a Main.list
  151 type 'a Main.List.t = 'a list
  154 mod Main.List
  259 val Main.List.empty : 'a t * 'b t
  272 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  $ sherlodoc search ": (int, 'a) result"
  val Main.ok_zero : (int, 'a) result
