  $ find . -name '*.odocl' | sort
  ./base.odocl
  ./base_internalhash_types.odocl
  ./caml.odocl
  ./md5_lib.odocl
  ./page-index.odocl
  ./shadow_stdlib.odocl
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.8M	megaodocl
  $ time sherlodoc_index --format=js --db=db.js $(find . -name '*.odocl')
  
  real	0m1.170s
  user	0m1.122s
  sys	0m0.043s





















$ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ gzip -k db.js

  $ gzip -k megaodocl

  $ du -s *.js *.gz
  2644	db.js
  1996	db.js.gz
  1544	megaodocl.gz


  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --search-uri=db.js --search-uri=sherlodoc.js --output-dir html $f
  > done
  $ odoc support-files -o html
  $ cp db.js html/
  $ cp ../../../jsoo/main.bc.js html/sherlodoc.js
  $ du -sh html/sherlodoc.js
  5.2M	html/sherlodoc.js
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
  $ sherlodoc --print-cost --no-rhs --db=db_marshal.bin --limit 100 "S_poly"
  115 sig Base.Map.S_poly
  115 sig Base.Set.S_poly
  119 sig Base.Hashtbl.S_poly
  623 val Base.Set.S_poly.map
  623 val Base.Set.S_poly.mem
  625 mod Base.Set.S_poly.Named
  627 val Base.Hashtbl.S_poly.add
  627 val Base.Hashtbl.S_poly.map
  627 val Base.Hashtbl.S_poly.set
  628 val Base.Hashtbl.S_poly.data
  628 val Base.Hashtbl.S_poly.find
  628 val Base.Hashtbl.S_poly.fold
  628 val Base.Hashtbl.S_poly.keys
  628 val Base.Hashtbl.S_poly.mapi
  630 val Base.Hashtbl.S_poly.choose
  632 val Base.Hashtbl.S_poly.find_exn
  632 val Base.Hashtbl.S_poly.to_alist
  634 val Base.Hashtbl.S_poly.choose_exn
  721 type ('a, 'b) Base.Map.S_poly.t
  721 type 'elt Base.Set.S_poly.t
  723 val Base.Map.S_poly.add
  723 val Base.Map.S_poly.map
  723 val Base.Map.S_poly.mem
  723 val Base.Map.S_poly.nth
  723 val Base.Map.S_poly.set
  723 val Base.Set.S_poly.add
  723 val Base.Set.S_poly.nth
  723 type ('a, 'cmp) Base.Set.S_poly.set
  723 val Base.Set.S_poly.sum
  724 val Base.Map.S_poly.data
  724 val Base.Map.S_poly.find
  724 val Base.Map.S_poly.fold
  724 val Base.Map.S_poly.iter
  724 val Base.Map.S_poly.keys
  724 val Base.Map.S_poly.mapi
  724 val Base.Map.S_poly.rank
  724 type ('a, 'b) Base.Map.S_poly.tree
  724 val Base.Set.S_poly.diff
  724 val Base.Set.S_poly.find
  724 val Base.Set.S_poly.fold
  724 val Base.Set.S_poly.iter
  724 type 'elt Base.Set.S_poly.tree
  725 type ('a, 'b) Base.Hashtbl.S_poly.t
  725 val Base.Map.S_poly.count
  725 val Base.Map.S_poly.empty
  725 val Base.Map.S_poly.iteri
  725 val Base.Set.S_poly.count
  725 val Base.Set.S_poly.empty
  725 val Base.Set.S_poly.equal
  725 val Base.Set.S_poly.inter
  725 val Base.Set.S_poly.iter2
  725 val Base.Set.S_poly.split
  725 val Base.Set.S_poly.union
  726 val Base.Map.S_poly.append
  726 val Base.Map.S_poly.exists
  726 val Base.Map.S_poly.length
  726 val Base.Map.S_poly.remove
  726 val Base.Set.S_poly.choose
  726 val Base.Set.S_poly.exists
  726 val Base.Set.S_poly.filter
  726 val Base.Set.S_poly.length
  726 val Base.Set.S_poly.remove
  727 type 'a Base.Hashtbl.S_poly.key
  727 val Base.Hashtbl.S_poly.mem
  727 val Base.Map.S_poly.add_exn
  727 val Base.Map.S_poly.max_elt
  727 val Base.Map.S_poly.min_elt
  727 val Base.Map.S_poly.nth_exn
  727 val Base.Map.S_poly.of_tree
  727 val Base.Map.S_poly.to_tree
  727 val Base.Set.S_poly.for_all
  727 val Base.Set.S_poly.max_elt
  727 val Base.Set.S_poly.min_elt
  727 val Base.Set.S_poly.of_list
  727 val Base.Set.S_poly.of_tree
  727 val Base.Set.S_poly.to_list
  727 val Base.Set.S_poly.to_tree
  728 val Base.Hashtbl.S_poly.copy
  728 val Base.Map.S_poly.find_exn
  728 val Base.Map.S_poly.is_empty
  728 val Base.Map.S_poly.map_keys
  728 val Base.Map.S_poly.of_alist
  728 val Base.Set.S_poly.elements
  728 val Base.Set.S_poly.find_exn
  728 val Base.Set.S_poly.is_empty
  728 val Base.Set.S_poly.of_array
  728 val Base.Set.S_poly.to_array
  729 val Base.Hashtbl.S_poly.clear
  729 val Base.Map.S_poly.singleton
  729 val Base.Set.S_poly.is_subset
  729 val Base.Set.S_poly.singleton
  730 val Base.Hashtbl.S_poly.length
  730 val Base.Map.S_poly.invariants
  730 val Base.Set.S_poly.choose_exn
  730 val Base.Set.S_poly.invariants
  731 val Base.Map.S_poly.max_elt_exn
  731 val Base.Map.S_poly.min_elt_exn
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
  330 val Base.List.sort_and_group
  330 val Base.Hashtbl.Creators.group
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
  222 val Base.Either.First.map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t
  226 val Base.Applicative.Make.map2 : 'a X.t -> 'b X.t -> f:('a -> 'b -> 'c) -> 'c X.t
  227 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  227 val Base.Applicative.Make2.map2 : ('a, 'e) X.t -> ('b, 'e) X.t -> f:('a -> 'b -> 'c) -> ('c, 'e) X.t
  227 val Base.Applicative.Make3.map2 : ('a, 'd, 'e) X.t -> ('b, 'd, 'e) X.t -> f:('a -> 'b -> 'c) -> ('c, 'd, 'e) X.t
  228 val Base.Applicative.Pair.F.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  228 val Base.Applicative.Pair.G.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  230 val Base.Applicative.Of_monad.map2 : 'a M.t -> 'b M.t -> f:('a -> 'b -> 'c) -> 'c M.t
  231 val Base.Uniform_array.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  231 val Base.Applicative.Compose.F.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  231 val Base.Applicative.Compose.G.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  231 val Base.Applicative.S_to_S2.X.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
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
  321 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  322 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  323 val Base.Array.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  340 type 'a Base.Applicative.Make_using_map2.X.t
  340 val Base.Applicative.Make_using_map2.map : 'a X.t -> f:('a -> 'b) -> 'b X.t
  341 type ('a, 'e) Base.Applicative.Make2_using_map2.X.t
  341 type ('a, 'd, 'e) Base.Applicative.Make3_using_map2.X.t
  342 val Base.Applicative.Make_using_map2.X.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  343 val Base.Applicative.Make2_using_map2.X.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  343 val Base.Applicative.Make3_using_map2.X.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  343 val Base.Applicative.Make_using_map2.return : 'a -> 'a X.t
  623 val Base.Applicative.S.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  624 val Base.Applicative.S2.map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t
  624 val Base.Applicative.S3.map2 : ('a, 'd, 'e) t -> ('b, 'd, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'd, 'e) t
  624 val Base.Either.Focused.map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t
  629 val Base.Applicative.S_local.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  739 type 'a Base.Applicative.Basic_using_map2.t
  740 type ('a, 'e) Base.Applicative.Basic2_using_map2.t
  740 type ('a, 'd, 'e) Base.Applicative.Basic3_using_map2.t
  741 val Base.Applicative.Basic_using_map2.map : [ `Define_using_map2 | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
  742 val Base.Applicative.Basic2_using_map2.map : [ `Define_using_map2 | `Custom of ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t ]
  742 val Base.Applicative.Basic3_using_map2.map : [ `Define_using_map2
  | `Custom of ('a, 'd, 'e) t -> f:('a -> 'b) -> ('b, 'd, 'e) t ]
  $ sherlodoc --print-cost --no-rhs --db=db_marshal.bin --static-sort "List map2"
  202 val Base.List.rev_map2_exn
  208 val Base.List.map2_exn
  215 val Base.List.map2
  239 val Base.List.rev_map2
  292 val Base.List.Cartesian_product.map2

  $ sherlodoc --no-rhs --db=db_marshal.bin "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group
