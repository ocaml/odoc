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
  Node node, (p='e') :: path
  Node node, (p='m') :: path
  Node node, (p='a') :: path
  Node node, (p='n') :: path
  Node node, (p='_') :: path
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  _, []
  val Main.unique_name : foo
  $ sherlodoc "multiple_hit"
  Node node, (p='t') :: path
  Node node, (p='i') :: path
  Node node, (p='h') :: path
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  _, []
  val Main.multiple_hit_1 : foo
  val Main.multiple_hit_2 : foo
  val Main.multiple_hit_3 : foo
  $ sherlodoc "name_conflict"
  Node node, (p='t') :: path
  Node node, (p='c') :: path
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  Leaf (x :: xs, outcome), y :: ys when x = y 
  _, []
  type Main.name_conflict = foo
  val Main.name_conflict : foo
  $ sherlodoc "nesting_priority"
  Node node, (p='y') :: path
  Node node, (p='t') :: path
  Node node, (p='i') :: path
  Node node, (p='r') :: path
  Node node, (p='o') :: path
  Node node, (p='i') :: path
  Node node, (p='r') :: path
  Node node, (p='p') :: path
  Node node, (p='_') :: path
  Node node, (p='g') :: path
  Node node, (p='n') :: path
  Node node, (p='i') :: path
  Node node, (p='t') :: path
  Node node, (p='s') :: path
  Node node, (p='e') :: path
  Node node, (p='n') :: path
  _, []
  val Main.nesting_priority : foo
  val Main.Nest.nesting_priority : foo
  $ sherlodoc --print-cost "list"
  Node node, (p='t') :: path
  Node node, (p='s') :: path
  Node node, (p='i') :: path
  Node node, (p='l') :: path
  _, []
  109 mod Main.List
  209 type Main.list
  315 type Main.List.t = 'a list
  317 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  319 val Main.Map.to_list : foo
  1108 val Main.foo : foo
  1154 doc page
  $ sherlodoc --print-cost "list map"
  Node node, (p='t') :: path
  Node node, (p='s') :: path
  Node node, (p='i') :: path
  Node node, (p='l') :: path
  _, []
  Node node, (p='p') :: path
  Node node, (p='a') :: path
  Node node, (p='m') :: path
  _, []
  317 val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  423 val Main.Map.to_list : foo
  2108 val Main.foo : foo
  $ sherlodoc --print-cost ":moo"
  Node node, (p='m') :: path
  Node node, (p='o') :: path
  Node node, (p='o') :: path
  Node node, (p='+') :: path
  _, []
  210 val Main.value : moo
  213 val Main.produce : unit -> moo
  217 val Main.produce_2' : unit -> unit -> moo
  $ sherlodoc --print-cost ":moo -> _"
  Node node, (p='m') :: path
  Node node, (p='o') :: path
  Node node, (p='o') :: path
  Node node, (p='-') :: path
  _, []
  212 val Main.consume : moo -> unit
  215 val Main.consume_2 : moo -> moo -> unit
  221 val Main.consume_2_other : moo -> t -> unit
  $ sherlodoc --print-cost "modtype"
  Node node, (p='e') :: path
  Node node, (p='p') :: path
  Node node, (p='y') :: path
  Node node, (p='t') :: path
  Node node, (p='d') :: path
  Node node, (p='o') :: path
  Node node, (p='m') :: path
  _, []
  112 sig Main.Modtype
  325 val Main.Modtype.v_modtype : foo
  $ sherlodoc --print-cost "S"
  Node node, (p='s') :: path
  _, []
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
  328 val Main.nesting_priority : foo
  333 val Main.Nest.nesting_priority : foo
  1108 val Main.foo : foo
  1154 doc page
  $ sherlodoc --print-cost "qwertyuiopasdfghjklzxcvbnm"
  Node node, (p='m') :: path
  Node node, (p='n') :: path
  [No results]
