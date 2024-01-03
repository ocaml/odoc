  $ find . -name '*.odocl' | sort
  ./base_odocls/base.odocl
  ./base_odocls/base_internalhash_types.odocl
  ./base_odocls/caml.odocl
  ./base_odocls/md5_lib.odocl
  ./base_odocls/page-index.odocl
  ./base_odocls/shadow_stdlib.odocl
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=ancient
  $ sherlodoc index --index-docstring=false $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc search --print-cost --limit 100 "S_poly"
  305 sig Base.Map.S_poly
  305 sig Base.Set.S_poly
  333 sig Base.Hashtbl.S_poly
  851 mod Base.Set.S_poly.Named
  858 val Base.Set.S_poly.mem : 'a t -> 'a -> bool
  875 val Base.Set.S_poly.map : ('a, _) set -> f:('a -> 'b) -> 'b t
  895 val Base.Hashtbl.S_poly.data : (_, 'b) t -> 'b list
  899 val Base.Hashtbl.S_poly.keys : ('a, _) t -> 'a key list
  908 val Base.Hashtbl.S_poly.map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
  908 val Base.Hashtbl.S_poly.find : ('a, 'b) t -> 'a key -> 'b option
  911 val Base.Hashtbl.S_poly.set : ('a, 'b) t -> key:'a key -> data:'b -> unit
  923 val Base.Hashtbl.S_poly.choose : ('a, 'b) t -> ('a key * 'b) option
  927 val Base.Hashtbl.S_poly.add : ('a, 'b) t -> key:'a key -> data:'b -> [ `Ok | `Duplicate ]
  929 val Base.Hashtbl.S_poly.find_exn : ('a, 'b) t -> 'a key -> 'b
  934 val Base.Hashtbl.S_poly.mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c) -> ('a, 'c) t
  935 val Base.Hashtbl.S_poly.to_alist : ('a, 'b) t -> ('a key * 'b) list
  936 val Base.Hashtbl.S_poly.equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  937 val Base.Hashtbl.S_poly.iteri : ('a, 'b) t -> f:(key:'a key -> data:'b -> unit) -> unit
  939 val Base.Hashtbl.S_poly.incr : ?by:int -> ?remove_if_zero:bool -> ('a, int) t -> 'a key -> unit
  940 val Base.Hashtbl.S_poly.update : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> unit
  942 val Base.Hashtbl.S_poly.choose_exn : ('a, 'b) t -> 'a key * 'b
  947 val Base.Hashtbl.S_poly.change : ('a, 'b) t -> 'a key -> f:('b option -> 'b option) -> unit
  951 val Base.Hashtbl.S_poly.fold : ('a, 'b) t -> init:'acc -> f:(key:'a key -> data:'b -> 'acc -> 'acc) -> 'acc
  953 val Base.Hashtbl.S_poly.find_multi : ('a, 'b list) t -> 'a key -> 'b list
  957 val Base.Hashtbl.S_poly.map_inplace : (_, 'b) t -> f:('b -> 'b) -> unit
  958 val Base.Hashtbl.S_poly.add_multi : ('a, 'b list) t -> key:'a key -> data:'b -> unit
  963 val Base.Hashtbl.S_poly.remove_multi : ('a, _ list) t -> 'a key -> unit
  964 val Base.Hashtbl.S_poly.filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t
  974 val Base.Hashtbl.S_poly.find_or_add : ('a, 'b) t -> 'a key -> default:(unit -> 'b) -> 'b
  983 val Base.Hashtbl.S_poly.findi_or_add : ('a, 'b) t -> 'a key -> default:('a key -> 'b) -> 'b
  985 val Base.Hashtbl.S_poly.find_and_remove : ('a, 'b) t -> 'a key -> 'b option
  986 val Base.Hashtbl.S_poly.partition_tf : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t * ('a, 'b) t
  990 val Base.Hashtbl.S_poly.filter_mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c option) -> ('a, 'c) t
  1005 mod Base.Map.S_poly.Make_applicative_traversals
  1006 val Base.Hashtbl.S_poly.partition_map : ('a, 'b) t -> f:('b -> ('c, 'd) Either.t) -> ('a, 'c) t * ('a, 'd) t
  1012 val Base.Hashtbl.S_poly.partitioni_tf : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> ('a, 'b) t * ('a, 'b) t
  1013 val Base.Hashtbl.S_poly.filter_map_inplace : (_, 'b) t -> f:('b -> 'b option) -> unit
  1015 val Base.Hashtbl.S_poly.update_and_return : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> 'b
  1018 val Base.Hashtbl.S_poly.choose_randomly : ?random_state:Random.State.t -> ('a, 'b) t -> ('a key * 'b) option
  1019 val Base.Hashtbl.S_poly.filter_keys_inplace : ('a, _) t -> f:('a key -> bool) -> unit
  1026 val Base.Hashtbl.S_poly.merge_into : src:('k, 'a) t ->
    dst:('k, 'b) t ->
    f:(key:'k key -> 'a -> 'b option -> 'b Merge_into_action.t) ->
    unit
  1026 val Base.Hashtbl.S_poly.find_and_call : ('a, 'b) t ->
    'a key ->
    if_found:('b -> 'c) ->
    if_not_found:('a key -> 'c) ->
    'c
  1027 val Base.Hashtbl.S_poly.merge : ('k, 'a) t ->
    ('k, 'b) t ->
    f:
      (key:'k key ->
        [ `Left of 'a | `Right of 'b | `Both of 'a * 'b ] ->
        'c option) ->
    ('k, 'c) t
  1036 val Base.Hashtbl.S_poly.partition_mapi : ('a, 'b) t ->
    f:(key:'a key -> data:'b -> ('c, 'd) Either.t) ->
    ('a, 'c) t * ('a, 'd) t
  1037 val Base.Hashtbl.S_poly.choose_randomly_exn : ?random_state:Random.State.t -> ('a, 'b) t -> 'a key * 'b
  1055 val Base.Hashtbl.S_poly.find_and_call1 : ('a, 'b) t ->
    'a key ->
    a:'d ->
    if_found:('b -> 'd -> 'c) ->
    if_not_found:('a key -> 'd -> 'c) ->
    'c
  1088 val Base.Hashtbl.S_poly.create_with_key : ?growth_allowed:bool ->
    ?size:int ->
    get_key:('r -> 'a key) ->
    'r list ->
    [ `Ok of ('a, 'r) t | `Duplicate_keys of 'a key list ]
  1092 val Base.Map.S_poly.Make_applicative_traversals.A.(<*>) : ('a -> 'b) t -> 'a t -> 'b t
  1099 val Base.Hashtbl.S_poly.create_mapped : ?growth_allowed:bool ->
    ?size:int ->
    get_key:('r -> 'a key) ->
    get_data:('r -> 'b) ->
    'r list ->
    [ `Ok of ('a, 'b) t | `Duplicate_keys of 'a key list ]
  1145 mod Base.Map.S_poly.Make_applicative_traversals.A.Applicative_infix
  1218 val Base.Map.S_poly.Make_applicative_traversals.A.Applicative_infix.(<*>) : ('a -> 'b) t -> 'a t -> 'b t
  1323 type ('a, 'b) Base.Map.S_poly.t
  1323 type 'elt Base.Set.S_poly.t
  1337 type ('a, 'cmp) Base.Set.S_poly.set
  1344 type ('a, 'b) Base.Map.S_poly.tree
  1344 type 'elt Base.Set.S_poly.tree
  1351 type ('a, 'b) Base.Hashtbl.S_poly.t
  1358 val Base.Set.S_poly.add : 'a t -> 'a -> 'a t
  1358 val Base.Set.S_poly.empty : 'a t
  1363 val Base.Map.S_poly.mem : ('k, _) t -> 'k -> bool
  1363 val Base.Map.S_poly.empty : ('k, _) t
  1364 val Base.Set.S_poly.nth : 'a t -> int -> 'a option
  1367 val Base.Map.S_poly.data : (_, 'v) t -> 'v list
  1367 val Base.Map.S_poly.keys : ('k, _) t -> 'k list
  1367 val Base.Set.S_poly.diff : 'a t -> 'a t -> 'a t
  1370 type 'a Base.Hashtbl.S_poly.key = 'a
  1371 val Base.Set.S_poly.length : _ t -> int
  1374 val Base.Set.S_poly.equal : 'a t -> 'a t -> bool
  1374 val Base.Set.S_poly.inter : 'a t -> 'a t -> 'a t
  1374 val Base.Set.S_poly.union : 'a t -> 'a t -> 'a t
  1376 val Base.Map.S_poly.find : ('k, 'v) t -> 'k -> 'v option
  1376 val Base.Map.S_poly.rank : ('k, _) t -> 'k -> int option
  1376 val Base.Map.S_poly.length : (_, _) t -> int
  1377 val Base.Map.S_poly.nth : ('k, 'v) t -> int -> ('k * 'v) option
  1377 val Base.Set.S_poly.iter : 'a t -> f:('a -> unit) -> unit
  1378 val Base.Set.S_poly.choose : 'a t -> 'a option
  1379 val Base.Set.S_poly.remove : 'a t -> 'a -> 'a t
  1382 val Base.Map.S_poly.iter : (_, 'v) t -> f:('v -> unit) -> unit
  1382 val Base.Set.S_poly.find : 'a t -> f:('a -> bool) -> 'a option
  1383 val Base.Set.S_poly.count : 'a t -> f:('a -> bool) -> int
  1383 val Base.Set.S_poly.of_list : 'a list -> 'a t
  1383 val Base.Set.S_poly.of_tree : 'a tree -> 'a t
  1383 val Base.Set.S_poly.to_list : 'a t -> 'a list
  1383 val Base.Set.S_poly.to_tree : 'a t -> 'a tree
  1384 val Base.Map.S_poly.map : ('k, 'v1) t -> f:('v1 -> 'v2) -> ('k, 'v2) t
  1385 val Base.Map.S_poly.set : ('k, 'v) t -> key:'k -> data:'v -> ('k, 'v) t
  1385 val Base.Set.S_poly.max_elt : 'a t -> 'a option
  1385 val Base.Set.S_poly.min_elt : 'a t -> 'a option
  1386 val Base.Set.S_poly.is_empty : _ t -> bool
  1389 val Base.Map.S_poly.count : ('k, 'v) t -> f:('v -> bool) -> int
  1390 val Base.Set.S_poly.elements : 'a t -> 'a list
  1391 val Base.Set.S_poly.split : 'a t -> 'a -> 'a t * 'a option * 'a t
  1391 val Base.Map.S_poly.remove : ('k, 'v) t -> 'k -> ('k, 'v) t
  1391 val Base.Set.S_poly.exists : 'a t -> f:('a -> bool) -> bool
  1391 val Base.Set.S_poly.filter : 'a t -> f:('a -> bool) -> 'a t
  1391 val Base.Map.S_poly.is_empty : (_, _) t -> bool
  1391 val Base.Set.S_poly.of_array : 'a array -> 'a t
  1391 val Base.Set.S_poly.to_array : 'a t -> 'a array
  1392 val Base.Set.S_poly.singleton : 'a -> 'a t
  1395 val Base.Hashtbl.S_poly.mem : ('a, _) t -> 'a key -> bool
  $ sherlodoc search --print-cost --no-rhs "group b"
  453 val Base.List.group
  484 val Base.Sequence.group
  514 val Base.List.Assoc.group
  571 val Base.List.groupi
  589 val Base.Hashtbl.group
  590 val Base.Set.group_by
  620 val Base.List.sort_and_group
  681 val Base.List.Assoc.sort_and_group
  1052 val Base.Hashtbl.Creators.group
  1109 val Base.Set.Poly.group_by
  1114 val Base.Hashtbl.Poly.group
  1136 val Base.Hashtbl.S_without_submodules.group
  1145 val Base.Hashtbl.Creators.group
  1209 val Base.Set.Using_comparator.group_by
  1244 val Base.Set.Using_comparator.Tree.group_by
  1523 val Base.Set.S_poly.group_by
  1528 val Base.Hashtbl.S_poly.group
  1624 val Base.Set.Accessors_generic.group_by
  1715 val Base.Set.Creators_and_accessors_generic.group_by
  $ sherlodoc search --no-rhs "group by"
  val Base.Set.group_by
  val Base.Set.Poly.group_by
  val Base.Set.Using_comparator.group_by
  val Base.Set.Using_comparator.Tree.group_by
  val Base.Set.S_poly.group_by
  val Base.Set.Accessors_generic.group_by
  val Base.Set.Creators_and_accessors_generic.group_by
  $ sherlodoc search --print-cost "map2"
  504 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  527 mod Base.Applicative.Make_using_map2
  534 sig Base.Applicative.Basic_using_map2
  534 mod Base.Applicative.Make2_using_map2
  534 mod Base.Applicative.Make3_using_map2
  538 val Base.Uniform_array.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  541 sig Base.Applicative.Basic2_using_map2
  541 sig Base.Applicative.Basic3_using_map2
  571 mod Base.Applicative.Make_using_map2_local
  578 sig Base.Applicative.Basic_using_map2_local
  578 mod Base.Applicative.Make2_using_map2_local
  578 mod Base.Applicative.Make3_using_map2_local
  585 sig Base.Applicative.Basic2_using_map2_local
  585 sig Base.Applicative.Basic3_using_map2_local
  607 val Base.Applicative.Make_using_map2.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  650 val Base.Applicative.Make_using_map2_local.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  654 mod Base.Applicative.Make_using_map2.Applicative_infix
  661 mod Base.Applicative.Make2_using_map2.Applicative_infix
  661 mod Base.Applicative.Make3_using_map2.Applicative_infix
  697 mod Base.Applicative.Make_using_map2_local.Applicative_infix
  704 mod Base.Applicative.Make2_using_map2_local.Applicative_infix
  704 mod Base.Applicative.Make3_using_map2_local.Applicative_infix
  733 val Base.Applicative.Make_using_map2.Applicative_infix.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  776 val Base.Applicative.Make_using_map2_local.Applicative_infix.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  857 val Base.Option.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  $ sherlodoc search --print-cost --no-rhs --static-sort "List map2"
  277 val Base.List.rev_map2_exn
  650 val Base.List.map2
  653 val Base.List.map2_exn
  674 val Base.List.rev_map2
  737 val Base.List.Cartesian_product.map2

  $ sherlodoc search --no-rhs "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group
  $ sherlodoc search --print-cost "list"
  263 mod Base.List
  263 mod Caml.List
  326 mod Shadow_stdlib.List
  409 mod Base.List.Assoc
  409 mod Base.List.Infix
  409 mod Base.ListLabels
  409 mod Caml.ListLabels
  410 val Base.List.rev : 'a t -> 'a t
  419 val Base.List.join : 'a t t -> 'a t
  422 val Base.List.last : 'a t -> 'a option
  424 val Base.List.drop : 'a t -> int -> 'a t
  424 val Base.List.take : 'a t -> int -> 'a t
  426 val Base.List.map : 'a t -> f:('a -> 'b) -> 'b t
  429 val Base.List.hd_exn : 'a t -> 'a
  429 val Base.List.return : 'a -> 'a t
  431 val Base.List.tl_exn : 'a t -> 'a t
  432 val Base.List.sub : 'a t -> pos:int -> len:int -> 'a t
  433 val Base.List.init : int -> f:(int -> 'a) -> 'a t
  433 val Base.List.concat : 'a t t -> 'a t
  435 val Base.List.bind : 'a t -> f:('a -> 'b t) -> 'b t
  438 val Base.List.(>>|) : 'a t -> ('a -> 'b) -> 'b t
  438 val Base.Set.to_list : ('a, _) t -> 'a list
  439 val Base.List.append : 'a t -> 'a t -> 'a t
  440 val Base.List.(>>=) : 'a t -> ('a -> 'b t) -> 'b t
  444 mod Base.List.Let_syntax
  $ sherlodoc search --print-cost ": list"
  320 val Base.Map.data : (_, 'v, _) t -> 'v list
  320 val Base.Map.keys : ('k, _, _) t -> 'k list
  337 val Base.Set.to_list : ('a, _) t -> 'a list
  344 val Base.Hashtbl.data : (_, 'b) t -> 'b list
  344 val Base.Set.elements : ('a, _) t -> 'a list
  344 val Base.Bytes.to_list : t -> char list
  346 val Base.String.split : t -> on:char -> t list
  348 val Base.Hashtbl.keys : ('a, _) t -> 'a key list
  376 val Base.String.split_lines : t -> t list
  378 val Base.Map.find_multi : ('k, 'v list, 'cmp) t -> 'k -> 'v list
  379 val Base.Hashtbl.Poly.data : (_, 'b) t -> 'b list
  379 val Base.String.to_list_rev : t -> char list
  383 val Base.Hashtbl.Poly.keys : ('a, _) t -> 'a key list
  384 val Base.Pretty_printer.all : unit -> string list
  385 val Base.Hashtbl.to_alist : ('a, 'b) t -> ('a key * 'b) list
  389 val Base.Set.group_by : ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list
  389 val Base.Sequence.split_n : 'a t -> int -> 'a list * 'a t
  392 val Base.Sequence.group : 'a t -> break:('a -> 'a -> bool) -> 'a list t
  394 val Base.Sequence.to_list_rev : 'a t -> 'a list
  401 val Base.Map.to_alist : ?key_order:[ `Increasing | `Decreasing ] -> ('k, 'v, _) t -> ('k * 'v) list
  403 val Base.Hashtbl.find_multi : ('a, 'b list) t -> 'a key -> 'b list
  404 val Base.Sequence.chunks_exn : 'a t -> int -> 'a list t
  410 val Base.Map.add_multi : ('k, 'v list, 'cmp) t -> key:'k -> data:'v -> ('k, 'v list, 'cmp) t
  793 val Base.Hashtbl.S_poly.data : (_, 'b) t -> 'b list
  797 val Base.Hashtbl.S_poly.keys : ('a, _) t -> 'a key list
