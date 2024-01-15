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
  195 val Base.Set.S_poly.mem : 'a t -> 'a -> bool
  202 val Base.Hashtbl.S_poly.data : (_, 'b) t -> 'b list
  206 val Base.Hashtbl.S_poly.keys : ('a, _) t -> 'a key list
  212 val Base.Set.S_poly.map : ('a, _) set -> f:('a -> 'b) -> 'b t
  212 val Base.Hashtbl.S_poly.find_exn : ('a, 'b) t -> 'a key -> 'b
  213 val Base.Hashtbl.S_poly.choose_exn : ('a, 'b) t -> 'a key * 'b
  215 sig Base.Map.S_poly
  215 sig Base.Set.S_poly
  215 val Base.Hashtbl.S_poly.find : ('a, 'b) t -> 'a key -> 'b option
  218 val Base.Hashtbl.S_poly.choose : ('a, 'b) t -> ('a key * 'b) option
  218 val Base.Hashtbl.S_poly.to_alist : ('a, 'b) t -> ('a key * 'b) list
  219 sig Base.Hashtbl.S_poly
  221 val Base.Hashtbl.S_poly.map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
  222 val Base.Hashtbl.S_poly.map_inplace : (_, 'b) t -> f:('b -> 'b) -> unit
  222 val Base.Hashtbl.S_poly.remove_multi : ('a, _ list) t -> 'a key -> unit
  224 val Base.Hashtbl.S_poly.set : ('a, 'b) t -> key:'a key -> data:'b -> unit
  224 val Base.Hashtbl.S_poly.find_multi : ('a, 'b list) t -> 'a key -> 'b list
  226 val Base.Hashtbl.S_poly.find_and_remove : ('a, 'b) t -> 'a key -> 'b option
  235 val Base.Hashtbl.S_poly.update : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> unit
  235 val Base.Hashtbl.S_poly.add_multi : ('a, 'b list) t -> key:'a key -> data:'b -> unit
  235 val Base.Hashtbl.S_poly.filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t
  236 val Base.Hashtbl.S_poly.filter_map_inplace : (_, 'b) t -> f:('b -> 'b option) -> unit
  236 val Base.Hashtbl.S_poly.filter_keys_inplace : ('a, _) t -> f:('a key -> bool) -> unit
  237 val Base.Hashtbl.S_poly.equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  238 val Base.Hashtbl.S_poly.iteri : ('a, 'b) t -> f:(key:'a key -> data:'b -> unit) -> unit
  239 val Base.Hashtbl.S_poly.find_or_add : ('a, 'b) t -> 'a key -> default:(unit -> 'b) -> 'b
  240 val Base.Hashtbl.S_poly.add : ('a, 'b) t -> key:'a key -> data:'b -> [ `Ok | `Duplicate ]
  241 val Base.Hashtbl.S_poly.mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c) -> ('a, 'c) t
  242 val Base.Hashtbl.S_poly.change : ('a, 'b) t -> 'a key -> f:('b option -> 'b option) -> unit
  242 val Base.Hashtbl.S_poly.findi_or_add : ('a, 'b) t -> 'a key -> default:('a key -> 'b) -> 'b
  244 val Base.Hashtbl.S_poly.update_and_return : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> 'b
  245 val Base.Hashtbl.S_poly.partition_tf : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t * ('a, 'b) t
  246 val Base.Hashtbl.S_poly.incr : ?by:int -> ?remove_if_zero:bool -> ('a, int) t -> 'a key -> unit
  254 val Base.Hashtbl.S_poly.choose_randomly_exn : ?random_state:Random.State.t -> ('a, 'b) t -> 'a key * 'b
  255 val Base.Hashtbl.S_poly.filter_mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c option) -> ('a, 'c) t
  258 val Base.Hashtbl.S_poly.fold : ('a, 'b) t -> init:'acc -> f:(key:'a key -> data:'b -> 'acc -> 'acc) -> 'acc
  259 val Base.Hashtbl.S_poly.partition_map : ('a, 'b) t -> f:('b -> ('c, 'd) Either.t) -> ('a, 'c) t * ('a, 'd) t
  259 val Base.Hashtbl.S_poly.choose_randomly : ?random_state:Random.State.t -> ('a, 'b) t -> ('a key * 'b) option
  265 val Base.Hashtbl.S_poly.partitioni_tf : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> ('a, 'b) t * ('a, 'b) t
  272 type ('a, 'b) Base.Map.S_poly.t
  272 type 'elt Base.Set.S_poly.t
  274 type ('a, 'cmp) Base.Set.S_poly.set
  275 type ('a, 'b) Base.Map.S_poly.tree
  275 type 'elt Base.Set.S_poly.tree
  276 type ('a, 'b) Base.Hashtbl.S_poly.t
  279 val Base.Hashtbl.S_poly.find_and_call : ('a, 'b) t ->
    'a key ->
    if_found:('b -> 'c) ->
    if_not_found:('a key -> 'c) ->
    'c
  283 val Base.Set.S_poly.empty : 'a t
  283 type 'a Base.Hashtbl.S_poly.key = 'a
  283 val Base.Hashtbl.S_poly.partition_mapi : ('a, 'b) t ->
    f:(key:'a key -> data:'b -> ('c, 'd) Either.t) ->
    ('a, 'c) t * ('a, 'd) t
  288 val Base.Map.S_poly.empty : ('k, _) t
  289 type Base.Map.S_poly.comparator_witness
  289 type Base.Set.S_poly.comparator_witness
  290 val Base.Set.S_poly.length : _ t -> int
  293 val Base.Set.S_poly.is_empty : _ t -> bool
  293 val Base.Set.S_poly.singleton : 'a -> 'a t
  294 val Base.Set.S_poly.choose_exn : 'a t -> 'a
  295 val Base.Set.S_poly.add : 'a t -> 'a -> 'a t
  295 val Base.Map.S_poly.length : (_, _) t -> int
  295 val Base.Set.S_poly.max_elt_exn : 'a t -> 'a
  295 val Base.Set.S_poly.min_elt_exn : 'a t -> 'a
  296 val Base.Set.S_poly.of_list : 'a list -> 'a t
  296 val Base.Set.S_poly.of_tree : 'a tree -> 'a t
  296 val Base.Set.S_poly.to_list : 'a t -> 'a list
  296 val Base.Set.S_poly.to_tree : 'a t -> 'a tree
  296 val Base.Set.S_poly.invariants : 'a t -> bool
  297 val Base.Set.S_poly.choose : 'a t -> 'a option
  297 val Base.Set.S_poly.elements : 'a t -> 'a list
  297 val Base.Hashtbl.S_poly.merge_into : src:('k, 'a) t ->
    dst:('k, 'b) t ->
    f:(key:'k key -> 'a -> 'b option -> 'b Merge_into_action.t) ->
    unit
  298 val Base.Map.S_poly.data : (_, 'v) t -> 'v list
  298 val Base.Map.S_poly.keys : ('k, _) t -> 'k list
  298 val Base.Set.S_poly.diff : 'a t -> 'a t -> 'a t
  298 val Base.Set.S_poly.remove : 'a t -> 'a -> 'a t
  298 val Base.Set.S_poly.max_elt : 'a t -> 'a option
  298 val Base.Set.S_poly.min_elt : 'a t -> 'a option
  298 val Base.Map.S_poly.is_empty : (_, _) t -> bool
  298 val Base.Set.S_poly.of_array : 'a array -> 'a t
  298 val Base.Set.S_poly.to_array : 'a t -> 'a array
  299 val Base.Set.S_poly.equal : 'a t -> 'a t -> bool
  299 val Base.Set.S_poly.inter : 'a t -> 'a t -> 'a t
  299 val Base.Set.S_poly.union : 'a t -> 'a t -> 'a t
  299 val Base.Hashtbl.S_poly.clear : (_, _) t -> unit
  299 val Base.Hashtbl.S_poly.length : (_, _) t -> int
  299 val Base.Hashtbl.S_poly.hashable : 'a Hashable.t
  300 val Base.Map.S_poly.mem : ('k, _) t -> 'k -> bool
  301 val Base.Set.S_poly.nth : 'a t -> int -> 'a option
  301 val Base.Set.S_poly.union_list : 'a t list -> 'a t
  302 val Base.Map.S_poly.invariants : ('k, 'v) t -> bool
  302 val Base.Hashtbl.S_poly.is_empty : (_, _) t -> bool
  302 val Base.Hashtbl.S_poly.find_and_call1 : ('a, 'b) t ->
    'a key ->
    a:'d ->
    if_found:('b -> 'd -> 'c) ->
    if_not_found:('a key -> 'd -> 'c) ->
    'c
  304 val Base.Map.S_poly.find_exn : ('k, 'v) t -> 'k -> 'v
  305 val Base.Map.S_poly.singleton : 'k -> 'v -> ('k, 'v) t
  305 val Base.Set.S_poly.remove_index : 'a t -> int -> 'a t
  306 val Base.Hashtbl.S_poly.copy : ('a, 'b) t -> ('a, 'b) t
  306 val Base.Map.S_poly.max_elt_exn : ('k, 'v) t -> 'k * 'v
  306 val Base.Map.S_poly.min_elt_exn : ('k, 'v) t -> 'k * 'v
  306 val Base.Set.S_poly.of_sequence : 'a Sequence.t -> 'a t
  306 val Base.Set.S_poly.are_disjoint : 'a t -> 'a t -> bool
  307 val Base.Map.S_poly.find : ('k, 'v) t -> 'k -> 'v option
  307 val Base.Map.S_poly.rank : ('k, _) t -> 'k -> int option
  307 val Base.Set.S_poly.compare_direct : 'a t -> 'a t -> int
  $ sherlodoc search --print-cost --no-rhs "group b"
  281 val Base.Set.group_by
  360 val Base.List.group
  367 val Base.Sequence.group
  375 val Base.Set.Poly.group_by
  390 val Base.List.Assoc.group
  390 val Base.List.sort_and_group
  403 val Base.Set.Using_comparator.group_by
  413 val Base.Set.Using_comparator.Tree.group_by
  420 val Base.List.Assoc.sort_and_group
  458 val Base.List.groupi
  477 val Base.Set.S_poly.group_by
  478 val Base.Hashtbl.group
  512 val Base.Set.Accessors_generic.group_by
  525 val Base.Set.Creators_and_accessors_generic.group_by
  578 val Base.Hashtbl.Poly.group
  585 val Base.Hashtbl.Creators.group
  592 val Base.Hashtbl.Creators.group
  604 val Base.Hashtbl.S_without_submodules.group
  680 val Base.Hashtbl.S_poly.group
  $ sherlodoc search --no-rhs "group by"
  val Base.Set.group_by
  val Base.Set.Poly.group_by
  val Base.Set.Using_comparator.group_by
  val Base.Set.Using_comparator.Tree.group_by
  val Base.Set.S_poly.group_by
  val Base.Set.Accessors_generic.group_by
  val Base.Set.Creators_and_accessors_generic.group_by
  $ sherlodoc search --print-cost "map2"
  142 val Base.Uniform_array.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  150 val Base.Applicative.Make_using_map2.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  157 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  173 val Base.Applicative.Make_using_map2.Applicative_infix.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  176 val Base.Applicative.Make_using_map2_local.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  199 val Base.Applicative.Make_using_map2_local.Applicative_infix.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  211 val Base.Option.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  213 val Base.Or_error.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  226 val Base.Applicative.Pair.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  229 val Base.Applicative.Compose.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  229 val Base.Applicative.S2_to_S.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  229 val Base.Applicative.S3_to_S.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  229 val Base.Applicative.Make_using_map2.return : 'a -> 'a X.t
  230 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  232 val Base.Applicative.Make.map2 : 'a X.t -> 'b X.t -> f:('a -> 'b -> 'c) -> 'c X.t
  232 val Base.List.Cartesian_product.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  233 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  234 val Base.Array.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  235 val Base.Applicative.Make2_using_map2.return : 'a -> ('a, _) X.t
  236 val Base.Applicative.Of_monad.map2 : 'a M.t -> 'b M.t -> f:('a -> 'b -> 'c) -> 'c M.t
  238 val Base.Applicative.Make3_using_map2.return : 'a -> ('a, _, _) X.t
  240 val Base.Either.First.map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t
  240 val Base.Applicative.Make_using_map2.all : 'a X.t list -> 'a list X.t
  241 val Base.Either.Second.map2 : ('a, 'e) t -> ('b, 'e) t -> f:('a -> 'b -> 'c) -> ('c, 'e) t
  243 val Base.Applicative.Make_using_map2.map2 : 'a X.t -> 'b X.t -> f:('a -> 'b -> 'c) -> 'c X.t

  $ sherlodoc search --print-cost --static-sort "List map2"
  97 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  193 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  210 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  212 val Base.List.Cartesian_product.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  214 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

  $ sherlodoc search --print-cost "List map2"
  177 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  250 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  252 val Base.List.Cartesian_product.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  253 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  274 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

  $ sherlodoc search --no-rhs "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group
  $ sherlodoc search --print-cost "list"
  105 val Base.Bytes.to_list : t -> char list
  106 val Base.Queue.of_list : 'a list -> 'a t
  106 val Base.Stack.of_list : 'a list -> 'a t
  109 val Base.Set.to_list : ('a, _) t -> 'a list
  110 val Base.Bytes.of_char_list : char list -> t
  113 val Base.Linked_queue.of_list : 'a list -> 'a t
  121 val Base.Info.of_list : ?trunc_after:int -> t list -> t
  122 val Base.Error.of_list : ?trunc_after:int -> t list -> t
  128 val Base.List.rev : 'a t -> 'a t
  129 val Base.List.hd_exn : 'a t -> 'a
  129 val Base.List.return : 'a -> 'a t
  130 val Base.Array.of_list_rev : 'a list -> 'a t
  130 val Base.String.to_list_rev : t -> char list
  131 val Base.List.join : 'a t t -> 'a t
  131 val Base.List.tl_exn : 'a t -> 'a t
  131 val Base.Sequence.shift_right_with_list : 'a t -> 'a list -> 'a t
  133 val Base.List.concat : 'a t t -> 'a t
  133 val Base.Sequence.to_list_rev : 'a t -> 'a list
  134 val Base.List.last : 'a t -> 'a option
  135 val Base.List.ignore_m : 'a t -> unit t
  136 val Base.List.drop : 'a t -> int -> 'a t
  136 val Base.List.take : 'a t -> int -> 'a t
  136 val Base.Sequence.cycle_list_exn : 'a list -> 'a t
  137 val Base.List.nth_exn : 'a t -> int -> 'a
  139 val Base.List.append : 'a t -> 'a t -> 'a t
  $ sherlodoc search --print-cost ": list"
  95 val Base.Bytes.to_list : t -> char list
  97 val Base.String.split_lines : t -> t list
  100 val Base.String.to_list_rev : t -> char list
  103 val Base.Sequence.to_list_rev : 'a t -> 'a list
  105 val Base.Pretty_printer.all : unit -> string list
  109 val Base.Set.to_list : ('a, _) t -> 'a list
  110 val Base.Hashtbl.data : (_, 'b) t -> 'b list
  110 val Base.Set.elements : ('a, _) t -> 'a list
  112 val Base.String.split : t -> on:char -> t list
  114 val Base.Hashtbl.keys : ('a, _) t -> 'a key list
  119 val Base.Map.data : (_, 'v, _) t -> 'v list
  119 val Base.Map.keys : ('k, _, _) t -> 'k list
  120 val Base.Hashtbl.Poly.data : (_, 'b) t -> 'b list
  124 val Base.Hashtbl.Poly.keys : ('a, _) t -> 'a key list
  126 val Base.String.split_on_chars : t -> on:char list -> t list
  136 val Base.Hashtbl.to_alist : ('a, 'b) t -> ('a key * 'b) list
  138 val Base.List.rev : 'a t -> 'a t
  139 val Base.List.return : 'a -> 'a t
  139 val Base.String.Search_pattern.split_on : t -> string -> string list
  141 val Base.List.join : 'a t t -> 'a t
  141 val Base.List.tl_exn : 'a t -> 'a t
  142 val Base.Hashtbl.find_multi : ('a, 'b list) t -> 'a key -> 'b list
  143 val Base.List.concat : 'a t t -> 'a t
  145 val Base.List.ignore_m : 'a t -> unit t
  146 val Base.Hashtbl.Poly.to_alist : ('a, 'b) t -> ('a key * 'b) list

Partial name search:
  $ sherlodoc search --print-cost "strin"
  186 val Base.Sexp.of_string : unit
  192 val Base.Exn.to_string : t -> string
  192 val Base.Sys.max_string_length : int
  194 val Base.Float.to_string : t -> string
  197 val Base.Exn.to_string_mach : t -> string
  197 val Base.Info.to_string_hum : t -> string
  197 val Base.Sign.to_string_hum : t -> string
  198 val Base.Error.to_string_hum : t -> string
  198 val Base.Info.to_string_mach : t -> string
  199 val Base.Error.to_string_mach : t -> string
  202 val Base.Or_error.error_string : string -> _ t
  204 val Base.Buffer.add_string : t -> string -> unit
  204 val Base.Sign_or_nan.to_string_hum : t -> string
  208 val Base.Info.to_string_hum_deprecated : t -> string
  209 val Base.Error.to_string_hum_deprecated : t -> string
  209 val Base.Float.to_padded_compact_string : t -> string
  209 val Base.Source_code_position.to_string : t -> string
  214 val Base.String.rev : t -> t
  215 val Base.Int.to_string_hum : ?delimiter:char -> t -> string
  217 val Base.String.hash : t -> int
  217 val Base.Int32.to_string_hum : ?delimiter:char -> t -> string
  217 val Base.Int63.to_string_hum : ?delimiter:char -> t -> string
  217 val Base.Int64.to_string_hum : ?delimiter:char -> t -> string
  218 val Base.String.escaped : t -> t
  218 val Base.String.max_length : int
  $ sherlodoc search --print-cost "tring"
  164 val Base.String.rev : t -> t
  166 val Base.Sexp.of_string : unit
  167 val Base.String.hash : t -> int
  168 val Base.String.escaped : t -> t
  168 val Base.String.max_length : int
  169 val Base.String.(^) : t -> t -> t
  170 val Base.String.uppercase : t -> t
  171 val Base.String.capitalize : t -> t
  172 val Base.Exn.to_string : t -> string
  172 val Base.String.append : t -> t -> t
  174 val Base.String.equal : t -> t -> bool
  174 val Base.String.prefix : t -> int -> t
  174 val Base.String.suffix : t -> int -> t
  174 val Base.Float.to_string : t -> string
  175 val Base.String.compare : t -> t -> int
  177 val Base.String.ascending : t -> t -> int
  177 val Base.String.split_lines : t -> t list
  179 val Base.String.drop_prefix : t -> int -> t
  179 val Base.String.drop_suffix : t -> int -> t
  179 val Base.String.common_prefix : t list -> t
  179 val Base.String.common_suffix : t list -> t
  180 val Base.String.to_list_rev : t -> char list
  180 val Base.String.common_prefix2 : t -> t -> t
  180 val Base.String.common_suffix2 : t -> t -> t
  182 val Base.Or_error.error_string : string -> _ t
