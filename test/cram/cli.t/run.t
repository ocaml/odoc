  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  8.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  Index_lib.main
  Indexing in 0.001179s
  trie_with_array_occ:0.00s
  trie_with_array:0.00s
  Cache.Elt_array_occ_trie.memo:0.00s
  Cache.Elt_array_trie.memo:0.00s
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
  109 mod Main.List
  209 type Main.list
  315 type Main.List.t = 'a list
  317 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  319 val Main.Map.to_list : foo
  1108 val Main.foo : foo
  1154 doc page
  $ sherlodoc --print-cost "list map"
  317 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  423 val Main.Map.to_list : foo
  2108 val Main.foo : foo
  $ sherlodoc --print-cost ":moo"
  210 val Main.value : moo
  213 val Main.produce : unit -> moo
  217 val Main.produce_2' : unit -> unit -> moo
  $ sherlodoc --print-cost ":moo -> _"
  212 val Main.consume : moo -> unit
  215 val Main.consume_2 : moo -> moo -> unit
  221 val Main.consume_2_other : moo -> t -> unit
  266 cons Main.MyExtension : moo -> extensible_type
  $ sherlodoc --print-cost "modtype"
  112 sig Main.Modtype
  325 val Main.Modtype.v_modtype : foo
  $ sherlodoc --print-cost "S"
  106 sig Main.S
  216 mod Main.List
  216 mod Main.Nest
  216 mod Main.S_to_S1
  316 type Main.list
  318 type Main.List.t = 'a list
  319 val Main.consume : moo -> unit
  320 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  321 val Main.consume_2 : moo -> moo -> unit
  323 val Main.Map.to_list : foo
  327 val Main.consume_2_other : moo -> t -> unit
  327 type Main.extensible_type = ..
  328 val Main.nesting_priority : foo
  333 val Main.Nest.nesting_priority : foo
  373 cons Main.MyExtension : moo -> extensible_type
  1108 val Main.foo : foo
  1154 doc page
  $ sherlodoc --print-cost "qwertyuiopasdfghjklzxcvbnm"
  [No results]
TODO : get a result for the query bellow
  $ sherlodoc --print-cost "hidden"
  [No results]
  $ sherlodoc --print-cost ":mo"
  217 val Main.value : moo
  220 val Main.produce : unit -> moo
  224 val Main.produce_2' : unit -> unit -> moo
  $ sherlodoc ":'a"
  val Main.poly_1 : 'a -> 'b -> 'c
  val Main.poly_2 : 'a -> 'b -> 'c -> 'a -> 'b -> 'c
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  $ sherlodoc ": 'a -> 'b -> 'c "
  [No results]
  $ sherlodoc ": ('a -> 'b) -> 'a t -> 'b t"
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
TODO : get a result for the query bellow
  $ sherlodoc ": 'a bo"
  [No results]
  $ sherlodoc ":extensible_type"
  cons Main.MyExtension : moo -> extensible_type
