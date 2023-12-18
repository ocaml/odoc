  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  $ export SHERLODOC_DB=db.bin
  $ sherlodoc --print-cost "list"
  109 mod Main.List
  209 type 'a Main.list
  315 type 'a Main.List.t = 'a list
  317 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  319 val Main.List.empty : 'a t * 'b t
  $ export OCAMLRUNPARAM=b
  $ sherlodoc ": (int, 'a) result"
  val Main.ok_zero : (int, 'a) result
