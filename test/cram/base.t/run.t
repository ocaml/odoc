q  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  5.1M	megaodocl
  $ time sherlodoc_index --format=js --db=db.js $(find . -name '*.odocl')
  Indexing in 2934.671164ms
  Export in 1175.176859ms
  
  real	0m5.095s
  user	0m4.675s
  sys	0m0.163s
$ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ gzip -k db.js

  $ gzip -k megaodocl

  $ du -s *.js *.gz
  2708	db.js
  2044	db.js.gz
  1624	megaodocl.gz


  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --search-file=db.js --search-file=sherlodoc.js --output-dir html $f 
  > done
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
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
  $ odoc support-files -o html
  $ cp db.js html/
  $ cp ../../../jsoo/main.bc.js html/sherlodoc.js
  $ du -sh html/sherlodoc.js
  13M	html/sherlodoc.js
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
  $ cp -r html /tmp
$ firefox /tmp/html/base/index.html
  $ sherlodoc_index --format=marshal --index-docstring=false --db=db_marshal.bin $(find . -name '*.odocl') 2> /dev/null
  Indexing in 1396.560907ms
  Export in 961.407900ms
  $ sherlodoc --db=db_marshal.bin "group b" | sort
  val Base.Hashtbl.group : ?growth_allowed:bool -> ?size:int -> 'a t -> get_key:('r -> 'a) -> get_data:('r -> 'b) -> combine:('b -> 'b -> 'b) -> 'r list -> [('a, 'b) t]
  val Base.List.Assoc.group : ('a * 'b) list -> equal:('a -> 'a -> bool) -> [('a, 'b list) t]
  val Base.List.Assoc.sort_and_group : ('a * 'b) list -> compare:('a -> 'a -> int) -> [('a, 'b list) t]
  val Base.List.group : 'a t -> break:('a -> 'a -> bool) -> 'a t t
  val Base.List.groupi : 'a t -> break:(int -> 'a -> 'a -> bool) -> 'a t t
  val Base.List.sort_and_group : 'a t -> compare:('a -> 'a -> int) -> 'a t t
  val Base.Sequence.group : 'a t -> break:('a -> 'a -> bool) -> 'a list t
  val Base.Set.Poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.S_poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.group_by : [('a, 'cmp) t] -> equiv:('a -> 'a -> bool) -> [('a, 'cmp) t] list
  $ sherlodoc --db=db_marshal.bin "group by"
  val Base.Set.group_by : [('a, 'cmp) t] -> equiv:('a -> 'a -> bool) -> [('a, 'cmp) t] list
  val Base.Set.S_poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.Poly.group_by : 'a t -> equiv:('a -> 'a -> bool) -> 'a t list
  val Base.Set.Using_comparator.group_by : [('a, 'cmp) t] -> equiv:('a -> 'a -> bool) -> [('a, 'cmp) t] list
  val Base.Set.Accessors_generic.group_by : [('a, 'cmp) t] -> equiv:('a elt -> 'a elt -> bool) -> [('a, 'cmp) t] list
  val Base.Set.Using_comparator.Tree.group_by : [('a, 'cmp) t] -> equiv:('a -> 'a -> bool) -> [('a, 'cmp) t] list
  val Base.Set.Creators_and_accessors_generic.group_by : [('a, 'cmp) t] -> equiv:('a elt -> 'a elt -> bool) -> [('a, 'cmp) t] list
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
  val Base.Hashtbl.S_without_submodules.group : ?growth_allowed:bool -> ?size:int -> 'a t -> get_key:('r -> 'a) -> get_data:('r -> 'b) -> combine:('b -> 'b -> 'b) -> 'r list -> [('a, 'b) t]
