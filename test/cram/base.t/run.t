  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  5.1M	megaodocl
  $ time sherlodoc_index --format=js --db=db.js $(find . -name '*.odocl')
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: {For_generated_code}1.t
  Warning, resolved hidden path: Base__.Hash_set_intf.M_sexp_grammar
  Warning, resolved hidden path: Base__.Hash_set_intf.M_sexp_grammar
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Either0.t
  Indexing in 802.123070ms
  Export in 625.782967ms
  
  real	0m1.817s
  user	0m1.735s
  sys	0m0.063s
$ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ gzip -k db.js

  $ gzip -k megaodocl

  $ du -s *.js *.gz
  2732	db.js
  2060	db.js.gz
  1628	megaodocl.gz


  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --with-search --output-dir html $f 2> /dev/null
  > done
  $ odoc support-files -o html
  $ cat db.js  ../../../jsoo/main.bc.js > html/index.js
  $ cp sherlodoc_db.bin html
  cp: cannot stat 'sherlodoc_db.bin': No such file or directory
  [1]
  $ du -sh html/index.js
  15M	html/index.js
  $ ls html
  base
  fonts
  highlight.pack.js
  index.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  $ cp -r html /tmp
$ firefox /tmp/html/base/index.html
  $ sherlodoc_index --format=marshal --index-docstring=false --db=db_marshal.bin $(find . -name '*.odocl') 2> /dev/null
  Indexing in 1257.050037ms
  Export in 1118.567944ms
  $ sherlodoc --db=db_marshal.bin "group b" | sort
    'a Key.t ->
    'r list ->
    ('a, 'b) t
    ?size:int ->
    combine:('b -> 'b -> 'b) ->
    get_data:('r -> 'b) ->
    get_key:('r -> 'a) ->
  val Base.Hashtbl.group : ?growth_allowed:bool ->
  val Base.List.Assoc.group : ('a * 'b) list -> equal:('a -> 'a -> bool) -> ('a, 'b list) t
  val Base.List.Assoc.sort_and_group : ('a * 'b) list -> compare:('a -> 'a -> int) -> ('a, 'b list) t
  val Base.List.group : 'a t -> break:('a -> 'a -> bool) -> 'a t t
  val Base.List.groupi : 'a t -> break:(int -> 'a -> 'a -> bool) -> 'a t t
  val Base.List.sort_and_group : 'a t -> compare:('a -> 'a -> int) -> 'a t t
  val Base.Sequence.group : 'a t -> break:('a -> 'a -> bool) -> 'a list t
  val Base.Set.Poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.S_poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  $ sherlodoc --db=db_marshal.bin "group by"
  val Base.Set.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  val Base.Set.Poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.Accessors_generic.group_by : ('a, 'cmp) t -> equiv:('a elt -> 'a elt -> bool) -> ('a, 'cmp) t list
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
  $ sherlodoc --db=db_marshal.bin "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group : ?growth_allowed:bool ->
    ?size:int ->
    'a Key.t ->
    get_key:('r -> 'a) ->
    get_data:('r -> 'b) ->
    combine:('b -> 'b -> 'b) ->
    'r list ->
    ('a, 'b) t
