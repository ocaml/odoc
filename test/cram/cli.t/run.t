  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  8.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
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
  $ sherlodoc "list"
  mod Main.List
  type 'a Main.list
  type 'a Main.List.t = 'a list
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.Map.to_list : foo
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val Main.foo : foo
  doc 
  $ sherlodoc "map"
  mod Main.Map
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.Map.to_list : foo
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val Main.foo : foo
  $ sherlodoc "list map"
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.Map.to_list : foo
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val Main.foo : foo
  $ sherlodoc "map2"
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc ":moo"
  val Main.value : moo
  val Main.produce : unit -> moo
  val Main.produce_2' : unit -> unit -> moo
  $ sherlodoc ":_ -> moo"
  val Main.produce : unit -> moo
  val Main.produce_2' : unit -> unit -> moo
  val Main.value : moo
  $ sherlodoc ":moo -> _"
  val Main.consume : moo -> unit
  val Main.consume_2 : moo -> moo -> unit
  val Main.consume_2_other : moo -> t -> unit
  cons Main.MyExtension : moo -> extensible_type
  $ sherlodoc "modtype"
  sig Main.Modtype
  val Main.Modtype.v_modtype : foo
  $ sherlodoc "S"
  sig Main.S
  mod Main.List
  mod Main.Nest
  mod Main.S_to_S1
  type 'a Main.list
  type 'a Main.List.t = 'a list
  val Main.consume : moo -> unit
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.consume_2 : moo -> moo -> unit
  val Main.Map.to_list : foo
  val Main.consume_2_other : moo -> t -> unit
  type Main.extensible_type = ..
  val Main.nesting_priority : foo
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val Main.Nest.nesting_priority : foo
  cons Main.MyExtension : moo -> extensible_type
  val Main.foo : foo
  doc 
  $ sherlodoc "qwertyuiopasdfghjklzxcvbnm"
  [No results]
TODO : get a result for the query bellow
  $ sherlodoc "hidden"
  [No results]
  $ sherlodoc ":mo"
  [No results]
  $ sherlodoc ":'a"
  val Main.poly_1 : 'a -> 'b -> 'c
  val Main.poly_2 : 'a -> 'b -> 'c -> 'a -> 'b -> 'c
  val Main.poly_param : 'a boo
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc ": 'a -> 'b -> 'c "
  val Main.poly_1 : 'a -> 'b -> 'c
  val Main.poly_2 : 'a -> 'b -> 'c -> 'a -> 'b -> 'c
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc ": ('a -> 'b) -> 'a t -> 'b t"
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
TODO : get a result for the query bellow
  $ sherlodoc ": 'a bo"
  [No results]
  $ sherlodoc ":extensible_type"
  cons Main.MyExtension : moo -> extensible_type
  $ sherlodoc ":exn"
  sherlodoc: internal error, uncaught exception:
             File "query/dynamic_cost.ml", line 328, characters 8-14: Assertion failed
             
  [125]
  $ sherlodoc ": exn_payload -> _"
  sherlodoc: internal error, uncaught exception:
             File "query/dynamic_cost.ml", line 328, characters 8-14: Assertion failed
             
  [125]
