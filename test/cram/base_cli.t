  $ find . -name '*.odocl' | sort
  ./base_odocls/base.odocl
  ./base_odocls/base_internalhash_types.odocl
  ./base_odocls/caml.odocl
  ./base_odocls/md5_lib.odocl
  ./base_odocls/page-index.odocl
  ./base_odocls/shadow_stdlib.odocl
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=ancient
  $ sherlodoc_index --index-docstring=false $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc --print-cost --no-rhs --limit 100 "S_poly"
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
  $ sherlodoc --print-cost --no-rhs "group b"
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
  $ sherlodoc --no-rhs "group by"
  val Base.Set.group_by
  val Base.Set.Poly.group_by
  val Base.Set.Using_comparator.group_by
  val Base.Set.Using_comparator.Tree.group_by
  val Base.Set.S_poly.group_by
  val Base.Set.Accessors_generic.group_by
  val Base.Set.Creators_and_accessors_generic.group_by
  $ sherlodoc --print-cost "map2"
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
  $ sherlodoc --print-cost --no-rhs --static-sort "List map2"
  202 val Base.List.rev_map2_exn
  208 val Base.List.map2_exn
  215 val Base.List.map2
  239 val Base.List.rev_map2
  292 val Base.List.Cartesian_product.map2

  $ sherlodoc --no-rhs "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group
  $ sherlodoc --print-cost "list"
  109 mod Base.List
  109 mod Caml.List
  118 mod Shadow_stdlib.List
  209 type 'a Base.list = 'a List.t
  216 type 'a Base.Export.list = 'a List.t
  217 val Base.List.map : 'a t -> f:('a -> 'b) -> 'b t
  217 val Base.List.mem : 'a t -> 'a -> equal:('a -> 'a -> bool) -> bool
  217 val Base.List.rev : 'a t -> 'a t
  217 val Base.List.sub : 'a t -> pos:int -> len:int -> 'a t
  217 val Base.List.sum : (module Container.Summable with type t = 'sum) ->
    'a t ->
    f:('a -> 'sum) ->
    'sum
  218 val Base.List.bind : 'a t -> f:('a -> 'b t) -> 'b t
  218 val Base.List.drop : 'a t -> int -> 'a t
  218 val Base.List.find : 'a t -> f:('a -> bool) -> 'a option
  218 val Base.List.fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
  218 val Base.List.init : int -> f:(int -> 'a) -> 'a t
  218 val Base.List.join : 'a t t -> 'a t
  218 val Base.List.last : 'a t -> 'a option
  218 val Base.List.mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
  218 val Base.List.sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
  218 val Base.List.take : 'a t -> int -> 'a t
  219 val Base.List.(>>|) : 'a t -> ('a -> 'b) -> 'b t
  219 mod Base.List.Assoc
  219 mod Base.List.Infix
  219 val Base.List.count : 'a t -> f:('a -> bool) -> int
  219 mod Base.ListLabels
  219 mod Caml.ListLabels
  219 val Base.Set.to_list : ('a, _) t -> 'a list
  220 val Base.List.append : 'a t -> 'a t -> 'a t
  220 val Base.List.concat : 'a t t -> 'a t
  220 val Base.List.hd_exn : 'a t -> 'a
  220 val Base.List.return : 'a -> 'a t
  220 val Base.List.tl_exn : 'a t -> 'a t
  221 val Base.List.nth_exn : 'a t -> int -> 'a
  221 val Base.Bytes.to_list : t -> char list
  221 val Base.Queue.of_list : 'a list -> 'a t
  221 val Base.Stack.of_list : 'a list -> 'a t
  224 mod Base.List.Let_syntax
  225 mod Base.List.Monad_infix
  228 mod Shadow_stdlib.ListLabels
  315 type 'a Base.List.t = 'a list
  316 val Base.List.hd : 'a t -> 'a option
  318 val Base.equal_list : 'a. ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
  320 val Base.compare_list : 'a. ('a -> 'a -> int) -> 'a list -> 'a list -> int
  320 val Base.sexp_of_list : 'a. ('a -> Sexplib0.Sexp.t) -> 'a list -> Sexplib0.Sexp.t
  321 type ('a, 'b) Base.List.Assoc.t = ('a * 'b) list
  321 val Base.list_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a list
  322 val Base.globalize_list : 'a. ('a -> 'a) -> 'a list -> 'a list
  322 val Base.hash_fold_list : 'a. (Hash.state -> 'a -> Hash.state) -> Hash.state -> 'a list -> Hash.state
  623 val Base.Queue.S.of_list : 'a list -> 'a t
  623 val Base.Stack.S.of_list : 'a list -> 'a t
  $ sherlodoc --print-cost ": list"
  116 val Base.Map.data : (_, 'v, _) t -> 'v list
  116 val Base.Map.keys : ('k, _, _) t -> 'k list
  118 val Base.Set.to_list : ('a, _) t -> 'a list
  119 val Base.Hashtbl.data : (_, 'b) t -> 'b list
  119 val Base.Hashtbl.keys : ('a, _) t -> 'a key list
  119 val Base.Set.elements : ('a, _) t -> 'a list
  119 val Base.String.split : t -> on:char -> t list
  119 val Base.Bytes.to_list : t -> char list
  121 val Base.Map.to_alist : ?key_order:[ `Increasing | `Decreasing ] -> ('k, 'v, _) t -> ('k * 'v) list
  123 val Base.Set.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  123 val Base.Map.find_multi : ('k, 'v list, 'cmp) t -> 'k -> 'v list
  124 val Base.Hashtbl.to_alist : ('a, 'b) t -> ('a key * 'b) list
  124 val Base.Hashtbl.Poly.data : (_, 'b) t -> 'b list
  124 val Base.Pretty_printer.all : unit -> string list
  124 val Base.String.split_lines : t -> t list
  124 val Base.String.to_list_rev : t -> char list
  126 val Base.Sequence.to_list_rev : 'a t -> 'a list
  210 val Caml.(@) : 'a list -> 'a list -> 'a list
  213 val Base.Bool.all : t list
  213 val Base.Char.all : t list
  213 val Base.Sign.all : t list
  213 val Base.Unit.all : t list
  216 val Base.Nothing.all : t list
  217 val Base.Ordering.all : t list
  218 val Base.List.to_list : 'a t -> 'a list
  219 val Shadow_stdlib.(@) : 'a list -> 'a list -> 'a list
  219 val Base.Array.to_list : 'a t -> 'a list
  219 val Base.Queue.to_list : 'a t -> 'a list
  219 val Base.Stack.to_list : 'a t -> 'a list
  220 val Base.Map.Poly.data : (_, 'v) t -> 'v list
  220 val Base.Map.Poly.keys : ('k, _) t -> 'k list
  220 val Base.Option.to_list : 'a t -> 'a list
  220 val Base.String.to_list : t -> elt list
  220 val Base.Float.Class.all : t list
  220 val Base.Sign_or_nan.all : t list
  221 val Base.Lazy.all : 'a t list -> 'a list t
  221 val Base.List.all : 'a t list -> 'a list t
  222 val Base.Sequence.to_list : 'a t -> 'a list
  222 val Base.Set.Poly.to_list : 'a t -> 'a list
  223 val Base.Option.all : 'a t list -> 'a list t
  225 val Base.Result.all : ('a, 'e) t list -> ('a list, 'e) t
  227 val Base.Monad.Make.all : 'a X.t list -> 'a list X.t
  526 val Base.Hashtbl.S_poly.data : (_, 'b) t -> 'b list
  526 val Base.Hashtbl.S_poly.keys : ('a, _) t -> 'a key list
  621 val Base.Queue.S.to_list : 'a t -> 'a list
  621 val Base.Stack.S.to_list : 'a t -> 'a list
  622 val Base.Map.S_poly.data : (_, 'v) t -> 'v list
  622 val Base.Map.S_poly.keys : ('k, _) t -> 'k list
  624 val Base.Monad.S.all : 'a t list -> 'a list t
  627 val Base.Monad.S2.all : ('a, 'e) t list -> ('a list, 'e) t
