  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl') 2> /dev/null > /dev/null
  $ export SHERLODOC_DB=db.bin
  $ sherlodoc "unique_name"
  val Main.unique_name : foo
  $ sherlodoc "multiple_hit"
  val Main.multiple_hit_1 : foo
  val Main.multiple_hit_2 : foo
  val Main.multiple_hit_3 : foo
  $ sherlodoc "name_conflict"
  type Main.name_conflict = foo
  val Main.name_conflict : foo
  $ sherlodoc "nesting_priority"
  val Main.nesting_priority : foo
  val Main.Nest.nesting_priority : foo
  $ sherlodoc --print-cost "list"
  109 module Main.List
  209 type Main.list
  215 type Main.List.t = 'a list
  217 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  219 val Main.Map.to_list : foo
  1108 val Main.foo : foo
  1154 doc page
  $ sherlodoc --print-cost "list map"
  217 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  223 val Main.Map.to_list : foo
  2108 val Main.foo : foo
  $ sherlodoc --print-cost ":moo"
  210 val Main.value : moo
  213 val Main.produce : unit -> moo
  217 val Main.produce_2' : unit -> unit -> moo
  $ sherlodoc --print-cost ":moo -> _"
  212 val Main.consume : moo -> unit
  215 val Main.consume_2 : moo -> moo -> unit
  221 val Main.consume_2_other : moo -> t -> unit
