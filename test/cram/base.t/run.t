  $ find . -name '*.odocl'
  ./caml.odocl
  ./md5_lib.odocl
  ./base.odocl
  ./shadow_stdlib.odocl
  ./page-index.odocl
  ./base_internalhash_types.odocl
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.8M	megaodocl
  $ time sherlodoc_index --format=js --db=db.js $(find . -name '*.odocl')

  real	0m3.077s
  user	0m2.886s
  sys	0m0.122s

$ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ gzip -k db.js

  $ gzip -k megaodocl

  $ du -s *.js *.gz
  2700	db.js
  2036	db.js.gz
  1544	megaodocl.gz


  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --search-uri=db.js --search-uri=sherlodoc.js --output-dir html $f
  > done
  $ odoc support-files -o html
  $ cp db.js html/
  $ cp ../../../jsoo/main.bc.js html/sherlodoc.js
  $ du -sh html/sherlodoc.js
  5.1M	html/sherlodoc.js
  $ ls html
  base
  db.js
  fonts
  highlight.pack.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  sherlodoc.js
indent to see results
$ cp -r html /tmp
$ firefox /tmp/html/base/index.html
  $ sherlodoc_index --format=marshal --index-docstring=false --db=db_marshal.bin $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc --db=db_marshal.bin "group b"
  val Base.List.group : 'a t -> break:('a -> 'a -> bool) -> 'a t t
  val Base.Sequence.group : 'a t -> break:('a -> 'a -> bool) -> 'a list t
  val Base.List.groupi : 'a t -> break:(int -> 'a -> 'a -> bool) -> 'a t t
  val Base.List.sort_and_group : 'a t -> compare:('a -> 'a -> int) -> 'a t t
  val Base.Set.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  val Base.List.Assoc.group : ('a * 'b) list -> equal:('a -> 'a -> bool) -> ('a, 'b list) t
  val Base.List.Assoc.sort_and_group : ('a * 'b) list -> compare:('a -> 'a -> int) -> ('a, 'b list) t
  val Base.Set.S_poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.Poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Hashtbl.group : ?growth_allowed:bool ->
    ?size:int ->
    'a Key.t ->
    get_key:('r -> 'a) ->
    get_data:('r -> 'b) ->
    combine:('b -> 'b -> 'b) ->
    'r list ->
    ('a, 'b) t
  val Base.Hashtbl.Creators.group : ?growth_allowed:bool ->
    ?size:int ->
    'a Key.t ->
    get_key:('r -> 'a) ->
    get_data:('r -> 'b) ->
    combine:('b -> 'b -> 'b) ->
    'r list ->
    ('a, 'b) t
  val Base.Set.Using_comparator.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  val Base.Set.Accessors_generic.group_by : ('a, 'cmp) t -> equiv:('a elt -> 'a elt -> bool) -> ('a, 'cmp) t list
  val Base.Hashtbl.S_poly.group : ?growth_allowed:bool ->
    ?size:int ->
    get_key:('r -> 'a key) ->
    get_data:('r -> 'b) ->
    combine:('b -> 'b -> 'b) ->
    'r list ->
    ('a, 'b) t
  val Base.Set.Using_comparator.Tree.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  val Base.Hashtbl.Poly.group : ?growth_allowed:bool ->
    ?size:int ->
    get_key:('r -> 'a key) ->
    get_data:('r -> 'b) ->
    combine:('b -> 'b -> 'b) ->
    'r list ->
    ('a, 'b) t
  val Base.Hashtbl.Creators.group : ?growth_allowed:bool ->
    ?size:int ->
    get_key:('r -> 'a Key.t) ->
    get_data:('r -> 'b) ->
    combine:('b -> 'b -> 'b) ->
    'r list ->
    ('a, 'b) t_
  val Base.Hashtbl.S_without_submodules.group : ?growth_allowed:bool ->
    ?size:int ->
    'a Key.t ->
    get_key:('r -> 'a) ->
    get_data:('r -> 'b) ->
    combine:('b -> 'b -> 'b) ->
    'r list ->
    ('a, 'b) t
  val Base.Set.Creators_and_accessors_generic.group_by : ('a, 'cmp) t -> equiv:('a elt -> 'a elt -> bool) -> ('a, 'cmp) t list
  $ sherlodoc --db=db_marshal.bin "group by"
  val Base.Set.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  val Base.Set.S_poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.Poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.Using_comparator.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  val Base.Set.Accessors_generic.group_by : ('a, 'cmp) t -> equiv:('a elt -> 'a elt -> bool) -> ('a, 'cmp) t list
  val Base.Set.Using_comparator.Tree.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  val Base.Set.Creators_and_accessors_generic.group_by : ('a, 'cmp) t -> equiv:('a elt -> 'a elt -> bool) -> ('a, 'cmp) t list
  $ sherlodoc --db=db_marshal.bin "map2"
  mod Base.Applicative.Make_using_map2
  mod Base.Applicative.Make2_using_map2
  mod Base.Applicative.Make3_using_map2
  sig Base.Applicative.Basic_using_map2
  sig Base.Applicative.Basic2_using_map2
  sig Base.Applicative.Basic3_using_map2
  mod Base.Applicative.Make_using_map2_local
  mod Base.Applicative.Make2_using_map2_local
  mod Base.Applicative.Make3_using_map2_local
  sig Base.Applicative.Basic_using_map2_local
  sig Base.Applicative.Basic2_using_map2_local
  sig Base.Applicative.Basic3_using_map2_local
  mod Base.Applicative.Make_using_map2.Applicative_infix
  mod Base.Applicative.Make2_using_map2.Applicative_infix
  mod Base.Applicative.Make3_using_map2.Applicative_infix
  val Base.Applicative.Make_using_map2.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  mod Base.Applicative.Make_using_map2_local.Applicative_infix
  mod Base.Applicative.Make2_using_map2_local.Applicative_infix
  mod Base.Applicative.Make3_using_map2_local.Applicative_infix
  type Base.Applicative.Basic_using_map2.t
  val Base.Applicative.Make_using_map2_local.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  type Base.Applicative.Basic2_using_map2.t
  type Base.Applicative.Basic3_using_map2.t
  type Base.Applicative.Make_using_map2.X.t
  val Base.Applicative.Basic_using_map2.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  type Base.Applicative.Make2_using_map2.X.t
  type Base.Applicative.Make3_using_map2.X.t
  val Base.Applicative.Basic2_using_map2.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  val Base.Applicative.Basic3_using_map2.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  val Base.Applicative.Make_using_map2.X.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  val Base.Applicative.Make2_using_map2.X.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  val Base.Applicative.Make3_using_map2.X.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  type Base.Applicative.Basic_using_map2_local.t
  val Base.Uniform_array.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  type Base.Applicative.Basic2_using_map2_local.t
  type Base.Applicative.Basic3_using_map2_local.t
  type Base.Applicative.Make_using_map2_local.X.t
  val Base.Applicative.Basic_using_map2_local.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  type Base.Applicative.Make2_using_map2_local.X.t
  type Base.Applicative.Make3_using_map2_local.X.t
  val Base.Applicative.Basic2_using_map2_local.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  val Base.Applicative.Basic3_using_map2_local.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  val Base.Applicative.Make_using_map2_local.X.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  val Base.Applicative.Make_using_map2.return : 'a -> 'a X.t
  val Base.Applicative.Make2_using_map2_local.X.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  val Base.Applicative.Make3_using_map2_local.X.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  val Base.Applicative.Make_using_map2.X.return : 'a -> 'a t
  val Base.Applicative.Make_using_map2.Applicative_infix.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  val Base.Applicative.Make2_using_map2.return : 'a -> ('a, _) X.t
  $ sherlodoc --db=db_marshal.bin "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group : ?growth_allowed:bool ->
    ?size:int ->
    'a Key.t ->
    get_key:('r -> 'a) ->
    get_data:('r -> 'b) ->
    combine:('b -> 'b -> 'b) ->
    'r list ->
    ('a, 'b) t
