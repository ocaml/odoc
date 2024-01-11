  $ ocamlc -c main.mli -bin-annot -I .
  $ odoc compile -I . main.cmti
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  8.0K	megaodocl
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=ancient
  $ sherlodoc index $(find . -name '*.odocl')
  $ sherlodoc search "unique_name"
  val Main.unique_name : foo
  $ sherlodoc search "multiple_hit"
  val Main.multiple_hit_1 : foo
  val Main.multiple_hit_2 : foo
  val Main.multiple_hit_3 : foo
  $ sherlodoc search --print-cost "name_conflict"
  832 val Main.name_conflict : foo
  832 type Main.name_conflict = foo
  $ sherlodoc search "nesting_priority"
  val Main.nesting_priority : foo
  val Main.Nest.nesting_priority : foo
  $ sherlodoc search "list"
  mod Main.List
  type 'a Main.list
  type 'a Main.List.t = 'a list
  val Main.Map.to_list : foo
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val Main.foo : foo
  doc 
  $ sherlodoc search "map"
  mod Main.Map
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.Map.to_list : foo
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val Main.foo : foo
  $ sherlodoc search "list map"
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.Map.to_list : foo
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val Main.foo : foo
  $ sherlodoc search "map2"
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc search ":moo"
  val Main.value : moo
  val Main.produce : unit -> moo
  val Main.produce_2' : unit -> unit -> moo
  $ sherlodoc search ":_ -> moo"
  val Main.produce : unit -> moo
  val Main.produce_2' : unit -> unit -> moo
  val Main.value : moo
  $ sherlodoc search ":moo -> _"
  val Main.consume : moo -> unit
  val Main.consume_2 : moo -> moo -> unit
  val Main.consume_2_other : moo -> t -> unit
  cons Main.MyExtension : moo -> extensible_type
  $ sherlodoc search "modtype"
  sig Main.Modtype
  val Main.Modtype.v_modtype : foo
  $ sherlodoc search "S"
  sig Main.S
  mod Main.List
  mod Main.Nest
  mod Main.S_to_S1
  type 'a Main.list
  type 'a Main.List.t = 'a list
  val Main.consume : moo -> unit
  val Main.Map.to_list : foo
  val Main.consume_2 : moo -> moo -> unit
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  type Main.extensible_type = ..
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.nesting_priority : foo
  val Main.consume_2_other : moo -> t -> unit
  cons Main.MyExtension : moo -> extensible_type
  val Main.Nest.nesting_priority : foo
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val Main.foo : foo
  doc 
  doc 
  $ sherlodoc search "qwertyuiopasdfghjklzxcvbnm"
  [No results]
  $ sherlodoc search "hidden"
  [No results]
TODO : get a result for the query bellow
  $ sherlodoc search ":mo"
  val Main.value : moo
  val Main.produce : unit -> moo
  val Main.produce_2' : unit -> unit -> moo
  $ sherlodoc search ":'a"
  val Main.poly_1 : 'a -> 'b -> 'c
  val Main.poly_2 : 'a -> 'b -> 'c -> 'a -> 'b -> 'c
  val Main.poly_param : 'a boo
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc search ": 'a -> 'b -> 'c "
  val Main.poly_1 : 'a -> 'b -> 'c
  val Main.poly_2 : 'a -> 'b -> 'c -> 'a -> 'b -> 'c
  val Main.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc search ": ('a -> 'b) -> 'a t -> 'b t"
  val Main.List.map : ('a -> 'b) -> 'a t -> 'b t
  val Main.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
TODO : get a result for the query bellow
  $ sherlodoc search ": 'a bo"
  val Main.poly_param : 'a boo
  $ sherlodoc search ":extensible_type"
  cons Main.MyExtension : moo -> extensible_type
  $ sherlodoc search ":exn"
  exn Main.Explicit_exn : exn_payload -> exn
  exn Main.Implicit_exn : exn_payload -> exn
  cons Main.Very_explicit_exn : exn_payload -> exn
  $ sherlodoc search ": exn_payload -> _"
  exn Main.Explicit_exn : exn_payload -> exn
  exn Main.Implicit_exn : exn_payload -> exn
  cons Main.Very_explicit_exn : exn_payload -> exn
  $ sherlodoc search ": long_name_type"
  val Main.long_name_value : long_name_type
  $ sherlodoc search ": long_nam"
  val Main.long_name_value : long_name_type
  $ sherlodoc search "long_name"
  type Main.long_name_type
  val Main.long_name_value : long_name_type
