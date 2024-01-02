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
  263 mod Main.List
  763 type 'a Main.list
  891 type 'a Main.List.t = 'a list
  923 val Main.List.empty : 'a t * 'b t
  924 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  $ sherlodoc search ": (int, 'a) result"
  val Main.ok_zero : (int, 'a) result
