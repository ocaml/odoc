  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  8.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl')
  Indexing in 8.804083ms
  Export in 3.623009ms
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
  val Main.foo : foo
  type Main.list
  type Main.List.t = 'a list
  val Main.Map.to_list : foo
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  doc page
  $ sherlodoc "map"
  mod Main.Map
  val Main.foo : foo
  val Main.Map.to_list : foo
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc "list map"
  val Main.foo : foo
  val Main.Map.to_list : foo
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc "map2"
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc ":moo"
  val Main.value : moo
  val Main.produce : unit -> moo
  val Main.produce_2' : unit -> unit -> moo
  $ sherlodoc ":moo -> _"
  cons Main.MyExtension : moo -> extensible_type
  val Main.consume : moo -> unit
  val Main.consume_2 : moo -> moo -> unit
  val Main.consume_2_other : moo -> t -> unit
  $ sherlodoc "modtype"
  sig Main.Modtype
  val Main.Modtype.v_modtype : foo
  $ sherlodoc "S"
  sig Main.S
  mod Main.List
  mod Main.Nest
  val Main.foo : foo
  mod Main.S_to_S1
  type Main.list
  type Main.List.t = 'a list
  val Main.Map.to_list : foo
  cons Main.MyExtension : moo -> extensible_type
  val Main.consume : moo -> unit
  $ sherlodoc "qwertyuiopasdfghjklzxcvbnm"
  [No results]
TODO : get a result for the query bellow
  $ sherlodoc "hidden"
  [No results]
  $ sherlodoc ":mo"
  [No results]
  $ sherlodoc ":'a"
  val Main.poly_1 : 'a -> 'b -> 'c
  val Main.poly_param : 'a boo
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.poly_2 : 'a -> 'b -> 'c -> 'a -> 'b -> 'c
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
  exn Main.Explicit_exn : exn_payload -> exn
  exn Main.Implicit_exn : exn_payload -> exn
  cons Main.Very_explicit_exn : exn_payload -> exn
  $ sherlodoc ": exn_payload -> _"
  exn Main.Explicit_exn : exn_payload -> exn
  exn Main.Implicit_exn : exn_payload -> exn
  cons Main.Very_explicit_exn : exn_payload -> exn
