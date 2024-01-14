  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc link -I . main.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=ancient
  $ sherlodoc index $(find . -name '*.odocl')
  $ sherlodoc search --print-cost "list"
  154 type 'a Main.list
  221 type 'a Main.List.t = 'a list
  229 val Main.List.empty : 'a t * 'b t
  242 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  254 mod Main.List
  $ sherlodoc search ": (int, 'a) result"
  val Main.ok_zero : (int, 'a) result
