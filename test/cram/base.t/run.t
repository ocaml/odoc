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
  
  real	0m1.168s
  user	0m1.131s
  sys	0m0.030s















$ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ gzip -k db.js

  $ gzip -k megaodocl

  $ du -s *.js *.gz
  2736	db.js
  2064	db.js.gz
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
  $ sherlodoc --print-cost --no-rhs --db=db_marshal.bin "S_poly"
  115 sig Base.Map.S_poly
  115 sig Base.Set.S_poly
  119 sig Base.Hashtbl.S_poly
  623 val Base.Set.S_poly.map
  623 val Base.Set.S_poly.mem
  625 mod Base.Set.S_poly.Named
  627 val Base.Hashtbl.S_poly.add
  628 val Base.Hashtbl.S_poly.data
  628 val Base.Hashtbl.S_poly.keys
  721 type Base.Map.S_poly.t
  721 type Base.Set.S_poly.t
  723 val Base.Map.S_poly.add
  723 val Base.Map.S_poly.mem
  723 val Base.Set.S_poly.add
  723 val Base.Set.S_poly.nth
  723 type Base.Set.S_poly.set
  723 val Base.Set.S_poly.sum
  724 val Base.Map.S_poly.data
  724 val Base.Map.S_poly.keys
  724 type Base.Map.S_poly.tree
  724 val Base.Set.S_poly.diff
  724 type Base.Set.S_poly.tree
  725 type Base.Hashtbl.S_poly.t
  725 val Base.Map.S_poly.empty
  725 val Base.Set.S_poly.empty
  725 val Base.Set.S_poly.equal
  725 val Base.Set.S_poly.inter
  725 val Base.Set.S_poly.union
  726 val Base.Map.S_poly.length
  726 val Base.Set.S_poly.choose
  726 val Base.Set.S_poly.length
  726 val Base.Set.S_poly.remove
  727 type Base.Hashtbl.S_poly.key
  727 val Base.Set.S_poly.max_elt
  727 val Base.Set.S_poly.min_elt
  727 val Base.Set.S_poly.of_list
  727 val Base.Set.S_poly.of_tree
  727 val Base.Set.S_poly.to_list
  727 val Base.Set.S_poly.to_tree
  728 val Base.Map.S_poly.of_alist
  728 val Base.Set.S_poly.elements
  728 val Base.Set.S_poly.is_empty
  728 val Base.Set.S_poly.of_array
  728 val Base.Set.S_poly.to_array
  729 val Base.Set.S_poly.singleton
  730 val Base.Set.S_poly.choose_exn
  730 val Base.Set.S_poly.invariants
  731 val Base.Set.S_poly.max_elt_exn
  731 val Base.Set.S_poly.min_elt_exn
  732 val Base.Hashtbl.S_poly.hashable
  $ sherlodoc --print-cost --no-rhs --db=db_marshal.bin "group b"
  218 val Base.List.group
  221 val Base.Hashtbl.group
  222 val Base.Sequence.group
  224 val Base.List.Assoc.group
  323 val Base.List.groupi
  324 val Base.Set.group_by
  326 val Base.Hashtbl.Poly.group
  330 val Base.Hashtbl.Creators.group
  330 val Base.List.sort_and_group
  336 val Base.List.Assoc.sort_and_group
  429 val Base.Set.Poly.group_by
  441 val Base.Set.Using_comparator.group_by
  446 val Base.Set.Using_comparator.Tree.group_by
  630 val Base.Hashtbl.Creators.group
  642 val Base.Hashtbl.S_without_submodules.group
  728 val Base.Hashtbl.S_poly.group
  831 val Base.Set.S_poly.group_by
  842 val Base.Set.Accessors_generic.group_by
  855 val Base.Set.Creators_and_accessors_generic.group_by
  $ sherlodoc --no-rhs --db=db_marshal.bin "group by"
  val Base.Set.group_by
  val Base.Set.Poly.group_by
  val Base.Set.Using_comparator.group_by
  val Base.Set.Using_comparator.Tree.group_by
  val Base.Set.S_poly.group_by
  val Base.Set.Accessors_generic.group_by
  val Base.Set.Creators_and_accessors_generic.group_by
  $ sherlodoc --print-cost --db=db_marshal.bin "map2"
  214 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  216 val Base.Option.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  218 val Base.Or_error.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  226 val Base.Applicative.Make.map2 : 'a X.t -> 'b X.t -> f:('a -> 'b -> 'c) -> 'c X.t
  227 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  235 mod Base.Applicative.Make_using_map2
  236 sig Base.Applicative.Basic_using_map2
  236 mod Base.Applicative.Make2_using_map2
  236 mod Base.Applicative.Make3_using_map2
  237 sig Base.Applicative.Basic2_using_map2
  237 sig Base.Applicative.Basic3_using_map2
  242 val Base.Applicative.Make_using_map2.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  243 mod Base.Applicative.Make_using_map2_local
  244 sig Base.Applicative.Basic_using_map2_local
  244 mod Base.Applicative.Make2_using_map2_local
  244 mod Base.Applicative.Make3_using_map2_local
  245 sig Base.Applicative.Basic2_using_map2_local
  245 sig Base.Applicative.Basic3_using_map2_local
  254 mod Base.Applicative.Make_using_map2.Applicative_infix
  321 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  322 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  323 val Base.Array.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  340 type Base.Applicative.Make_using_map2.X.t
  340 val Base.Applicative.Make_using_map2.all : 'a X.t list -> 'a list X.t
  341 type Base.Applicative.Make2_using_map2.X.t
  341 type Base.Applicative.Make3_using_map2.X.t
  342 val Base.Applicative.Make_using_map2.X.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  343 val Base.Applicative.Make2_using_map2.X.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  343 val Base.Applicative.Make3_using_map2.X.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  343 val Base.Applicative.Make_using_map2.return : 'a -> 'a X.t
  345 val Base.Applicative.Make_using_map2.X.return : 'a -> 'a t
  347 type Base.Applicative.Make_using_map2_local.X.t
  348 type Base.Applicative.Make2_using_map2_local.X.t
  348 type Base.Applicative.Make3_using_map2_local.X.t
  349 val Base.Applicative.Make_using_map2_local.X.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  350 val Base.Applicative.Make2_using_map2_local.X.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  350 val Base.Applicative.Make3_using_map2_local.X.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  623 val Base.Applicative.S.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  739 type Base.Applicative.Basic_using_map2.t
  740 type Base.Applicative.Basic2_using_map2.t
  740 type Base.Applicative.Basic3_using_map2.t
  741 val Base.Applicative.Basic_using_map2.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  742 val Base.Applicative.Basic2_using_map2.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  742 val Base.Applicative.Basic3_using_map2.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  746 type Base.Applicative.Basic_using_map2_local.t
  747 type Base.Applicative.Basic2_using_map2_local.t
  747 type Base.Applicative.Basic3_using_map2_local.t
  748 val Base.Applicative.Basic_using_map2_local.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  749 val Base.Applicative.Basic2_using_map2_local.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  749 val Base.Applicative.Basic3_using_map2_local.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  $ sherlodoc --print-cost --no-rhs --db=db_marshal.bin --static-sort "List map2"
  261 val Base.List.rev_map2_exn
  267 val Base.List.map2_exn
  275 val Base.List.map2
  299 val Base.List.rev_map2
  351 val Base.List.Cartesian_product.map2

  $ sherlodoc --no-rhs --db=db_marshal.bin "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group
