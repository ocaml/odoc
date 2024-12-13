  $ ODOCLS=$(find ../docs/odoc/base/ -name '*.odocl' | grep -v "__")
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index --index-docstring=false $ODOCLS
  $ sherlodoc search --print-cost --limit 100 "S_poly"
  200 sig Base.Map.S_poly
  200 sig Base.Set.S_poly
  204 sig Base.Hashtbl.S_poly
  248 type 'a Base.Hashtbl.S_poly.key = 'a
  257 type ('a, 'b) Base.Map.S_poly.t
  257 type 'elt Base.Set.S_poly.t
  259 type ('a, 'cmp) Base.Set.S_poly.set
  260 val Base.Set.S_poly.mem : 'a t -> 'a -> bool
  260 type ('a, 'b) Base.Map.S_poly.tree
  260 type 'elt Base.Set.S_poly.tree
  261 type ('a, 'b) Base.Hashtbl.S_poly.t
  261 mod Base.Set.S_poly.Named
  267 val Base.Hashtbl.S_poly.data : (_, 'b) t -> 'b list
  271 val Base.Hashtbl.S_poly.keys : ('a, _) t -> 'a key list
  274 type Base.Map.S_poly.comparator_witness
  274 type Base.Set.S_poly.comparator_witness
  277 val Base.Set.S_poly.map : ('a, _) set -> f:('a -> 'b) -> 'b t
  277 val Base.Hashtbl.S_poly.find_exn : ('a, 'b) t -> 'a key -> 'b
  278 val Base.Hashtbl.S_poly.choose_exn : ('a, 'b) t -> 'a key * 'b
  280 val Base.Hashtbl.S_poly.find : ('a, 'b) t -> 'a key -> 'b option
  283 val Base.Hashtbl.S_poly.choose : ('a, 'b) t -> ('a key * 'b) option
  283 val Base.Hashtbl.S_poly.to_alist : ('a, 'b) t -> ('a key * 'b) list
  283 mod Base.Map.S_poly.Make_applicative_traversals
  286 val Base.Hashtbl.S_poly.map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
  287 val Base.Hashtbl.S_poly.map_inplace : (_, 'b) t -> f:('b -> 'b) -> unit
  287 val Base.Hashtbl.S_poly.remove_multi : ('a, _ list) t -> 'a key -> unit
  289 val Base.Hashtbl.S_poly.set : ('a, 'b) t -> key:'a key -> data:'b -> unit
  289 val Base.Hashtbl.S_poly.find_multi : ('a, 'b list) t -> 'a key -> 'b list
  291 val Base.Hashtbl.S_poly.find_and_remove : ('a, 'b) t -> 'a key -> 'b option
  300 val Base.Hashtbl.S_poly.update : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> unit
  300 val Base.Hashtbl.S_poly.add_multi : ('a, 'b list) t -> key:'a key -> data:'b -> unit
  300 val Base.Hashtbl.S_poly.filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t
  301 val Base.Hashtbl.S_poly.filter_map_inplace : (_, 'b) t -> f:('b -> 'b option) -> unit
  301 val Base.Hashtbl.S_poly.filter_keys_inplace : ('a, _) t -> f:('a key -> bool) -> unit
  302 val Base.Hashtbl.S_poly.equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  303 val Base.Hashtbl.S_poly.iteri : ('a, 'b) t -> f:(key:'a key -> data:'b -> unit) -> unit
  304 val Base.Hashtbl.S_poly.find_or_add : ('a, 'b) t -> 'a key -> default:(unit -> 'b) -> 'b
  305 val Base.Hashtbl.S_poly.add : ('a, 'b) t -> key:'a key -> data:'b -> [ `Ok | `Duplicate ]
  306 val Base.Hashtbl.S_poly.mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c) -> ('a, 'c) t
  307 val Base.Hashtbl.S_poly.change : ('a, 'b) t -> 'a key -> f:('b option -> 'b option) -> unit
  307 val Base.Hashtbl.S_poly.findi_or_add : ('a, 'b) t -> 'a key -> default:('a key -> 'b) -> 'b
  309 val Base.Hashtbl.S_poly.update_and_return : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> 'b
  310 val Base.Hashtbl.S_poly.partition_tf : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t * ('a, 'b) t
  311 val Base.Hashtbl.S_poly.incr : ?by:int -> ?remove_if_zero:bool -> ('a, int) t -> 'a key -> unit
  319 val Base.Hashtbl.S_poly.choose_randomly_exn : ?random_state:Random.State.t -> ('a, 'b) t -> 'a key * 'b
  320 val Base.Hashtbl.S_poly.filter_mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c option) -> ('a, 'c) t
  323 val Base.Hashtbl.S_poly.fold : ('a, 'b) t -> init:'acc -> f:(key:'a key -> data:'b -> 'acc -> 'acc) -> 'acc
  324 val Base.Hashtbl.S_poly.partition_map : ('a, 'b) t -> f:('b -> ('c, 'd) Either.t) -> ('a, 'c) t * ('a, 'd) t
  324 val Base.Hashtbl.S_poly.choose_randomly : ?random_state:Random.State.t -> ('a, 'b) t -> ('a key * 'b) option
  330 val Base.Hashtbl.S_poly.partitioni_tf : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> ('a, 'b) t * ('a, 'b) t
  344 val Base.Hashtbl.S_poly.find_and_call : ('a, 'b) t ->
    'a key ->
    if_found:('b -> 'c) ->
    if_not_found:('a key -> 'c) ->
    'c
  348 val Base.Set.S_poly.empty : 'a t
  348 val Base.Hashtbl.S_poly.partition_mapi : ('a, 'b) t ->
    f:(key:'a key -> data:'b -> ('c, 'd) Either.t) ->
    ('a, 'c) t * ('a, 'd) t
  353 val Base.Map.S_poly.empty : ('k, _) t
  355 val Base.Set.S_poly.length : _ t -> int
  358 val Base.Set.S_poly.is_empty : _ t -> bool
  358 val Base.Set.S_poly.singleton : 'a -> 'a t
  359 val Base.Set.S_poly.choose_exn : 'a t -> 'a
  360 val Base.Set.S_poly.add : 'a t -> 'a -> 'a t
  360 val Base.Map.S_poly.length : (_, _) t -> int
  360 val Base.Set.S_poly.max_elt_exn : 'a t -> 'a
  360 val Base.Set.S_poly.min_elt_exn : 'a t -> 'a
  361 val Base.Set.S_poly.of_list : 'a list -> 'a t
  361 val Base.Set.S_poly.of_tree : 'a tree -> 'a t
  361 val Base.Set.S_poly.to_list : 'a t -> 'a list
  361 val Base.Set.S_poly.to_tree : 'a t -> 'a tree
  361 val Base.Set.S_poly.invariants : 'a t -> bool
  362 val Base.Set.S_poly.choose : 'a t -> 'a option
  362 val Base.Set.S_poly.elements : 'a t -> 'a list
  362 val Base.Hashtbl.S_poly.merge_into : src:('k, 'a) t ->
    dst:('k, 'b) t ->
    f:(key:'k key -> 'a -> 'b option -> 'b Merge_into_action.t) ->
    unit
  363 val Base.Map.S_poly.data : (_, 'v) t -> 'v list
  363 val Base.Map.S_poly.keys : ('k, _) t -> 'k list
  363 val Base.Set.S_poly.diff : 'a t -> 'a t -> 'a t
  363 val Base.Set.S_poly.remove : 'a t -> 'a -> 'a t
  363 val Base.Set.S_poly.max_elt : 'a t -> 'a option
  363 val Base.Set.S_poly.min_elt : 'a t -> 'a option
  363 val Base.Map.S_poly.is_empty : (_, _) t -> bool
  363 val Base.Set.S_poly.of_array : 'a array -> 'a t
  363 val Base.Set.S_poly.to_array : 'a t -> 'a array
  364 val Base.Set.S_poly.equal : 'a t -> 'a t -> bool
  364 val Base.Set.S_poly.inter : 'a t -> 'a t -> 'a t
  364 val Base.Set.S_poly.union : 'a t -> 'a t -> 'a t
  364 val Base.Hashtbl.S_poly.clear : (_, _) t -> unit
  364 val Base.Hashtbl.S_poly.length : (_, _) t -> int
  364 val Base.Hashtbl.S_poly.hashable : 'a Hashable.t
  365 val Base.Map.S_poly.mem : ('k, _) t -> 'k -> bool
  366 val Base.Set.S_poly.nth : 'a t -> int -> 'a option
  366 val Base.Set.S_poly.union_list : 'a t list -> 'a t
  367 val Base.Map.S_poly.invariants : ('k, 'v) t -> bool
  367 val Base.Hashtbl.S_poly.is_empty : (_, _) t -> bool
  367 val Base.Hashtbl.S_poly.find_and_call1 : ('a, 'b) t ->
    'a key ->
    a:'d ->
    if_found:('b -> 'd -> 'c) ->
    if_not_found:('a key -> 'd -> 'c) ->
    'c
  369 val Base.Map.S_poly.find_exn : ('k, 'v) t -> 'k -> 'v
  370 val Base.Map.S_poly.singleton : 'k -> 'v -> ('k, 'v) t
  370 val Base.Set.S_poly.remove_index : 'a t -> int -> 'a t
  371 val Base.Hashtbl.S_poly.copy : ('a, 'b) t -> ('a, 'b) t
  371 val Base.Map.S_poly.max_elt_exn : ('k, 'v) t -> 'k * 'v
  371 val Base.Map.S_poly.min_elt_exn : ('k, 'v) t -> 'k * 'v
  371 val Base.Set.S_poly.of_sequence : 'a Sequence.t -> 'a t
  371 val Base.Set.S_poly.are_disjoint : 'a t -> 'a t -> bool
  372 val Base.Set.S_poly.compare_direct : 'a t -> 'a t -> int
  $ sherlodoc search --print-cost --no-rhs "group b"
  231 val Base.Set.group_by
  255 val Base.List.group
  262 val Base.Sequence.group
  275 val Base.List.sort_and_group
  278 val Base.List.groupi
  285 val Base.List.Assoc.group
  305 val Base.List.Assoc.sort_and_group
  325 val Base.Set.Poly.group_by
  353 val Base.Set.Using_comparator.group_by
  363 val Base.Set.Using_comparator.Tree.group_by
  373 val Base.Hashtbl.group
  427 val Base.Set.S_poly.group_by
  462 val Base.Set.Accessors_generic.group_by
  473 val Base.Hashtbl.Poly.group
  475 val Base.Set.Creators_and_accessors_generic.group_by
  480 val Base.Hashtbl.Creators.group
  487 val Base.Hashtbl.Creators.group
  499 val Base.Hashtbl.S_without_submodules.group
  575 val Base.Hashtbl.S_poly.group
  $ sherlodoc search --no-rhs "group by"
  val Base.Set.group_by
  val Base.Set.Poly.group_by
  val Base.Set.Using_comparator.group_by
  val Base.Set.Using_comparator.Tree.group_by
  val Base.Set.S_poly.group_by
  val Base.Set.Accessors_generic.group_by
  val Base.Set.Creators_and_accessors_generic.group_by
  $ sherlodoc search --print-cost "map2"
  177 mod Base.Applicative.Make_using_map2
  178 mod Base.Applicative.Make2_using_map2
  178 mod Base.Applicative.Make3_using_map2
  188 mod Base.Applicative.Make_using_map2_local
  189 mod Base.Applicative.Make2_using_map2_local
  189 mod Base.Applicative.Make3_using_map2_local
  192 val Base.Uniform_array.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  197 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  200 mod Base.Applicative.Make_using_map2.Applicative_infix
  201 mod Base.Applicative.Make2_using_map2.Applicative_infix
  201 mod Base.Applicative.Make3_using_map2.Applicative_infix
  205 val Base.Applicative.Make_using_map2.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  211 mod Base.Applicative.Make_using_map2_local.Applicative_infix
  212 mod Base.Applicative.Make2_using_map2_local.Applicative_infix
  212 mod Base.Applicative.Make3_using_map2_local.Applicative_infix
  216 val Base.Applicative.Make_using_map2_local.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  228 sig Base.Applicative.Basic_using_map2
  228 val Base.Applicative.Make_using_map2.Applicative_infix.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  229 sig Base.Applicative.Basic2_using_map2
  229 sig Base.Applicative.Basic3_using_map2
  239 sig Base.Applicative.Basic_using_map2_local
  239 val Base.Applicative.Make_using_map2_local.Applicative_infix.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  240 sig Base.Applicative.Basic2_using_map2_local
  240 sig Base.Applicative.Basic3_using_map2_local
  276 val Base.Option.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  $ sherlodoc search --print-cost --static-sort "List map2"
  177 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  273 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  290 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  292 val Base.List.Cartesian_product.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  294 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

  $ sherlodoc search --print-cost "List map2"
  202 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  288 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  300 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  302 val Base.List.Cartesian_product.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  314 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

  $ sherlodoc search --no-rhs "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group
  $ sherlodoc search --print-cost "list"
  131 type 'a Base.list = 'a List.t
  143 type 'a Base.Export.list = 'a List.t
  151 type 'a Base.List.t = 'a list
  154 mod Base.List
  154 mod Caml.List
  158 val Base.List.rev : 'a t -> 'a t
  159 val Base.List.hd_exn : 'a t -> 'a
  159 val Base.List.return : 'a -> 'a t
  160 val Base.Bytes.to_list : t -> char list
  161 val Base.List.join : 'a t t -> 'a t
  161 val Base.List.tl_exn : 'a t -> 'a t
  161 val Base.Queue.of_list : 'a list -> 'a t
  161 val Base.Stack.of_list : 'a list -> 'a t
  163 val Base.List.concat : 'a t t -> 'a t
  163 mod Shadow_stdlib.List
  164 val Base.List.last : 'a t -> 'a option
  164 val Base.Set.to_list : ('a, _) t -> 'a list
  165 mod Base.List.Assoc
  165 mod Base.List.Infix
  165 cons Base.Sexp.t.List : t list -> t
  165 val Base.List.ignore_m : 'a t -> unit t
  165 val Base.Bytes.of_char_list : char list -> t
  166 val Base.List.drop : 'a t -> int -> 'a t
  166 val Base.List.take : 'a t -> int -> 'a t
  167 val Base.List.nth_exn : 'a t -> int -> 'a
  $ sherlodoc search --print-cost ": list"
  168 val Base.List.rev : 'a t -> 'a t
  169 val Base.List.return : 'a -> 'a t
  170 val Base.Bytes.to_list : t -> char list
  171 val Base.List.join : 'a t t -> 'a t
  171 val Base.List.tl_exn : 'a t -> 'a t
  172 val Base.String.split_lines : t -> t list
  173 val Base.List.concat : 'a t t -> 'a t
  175 val Base.List.ignore_m : 'a t -> unit t
  175 val Base.String.to_list_rev : t -> char list
  178 val Base.Sequence.to_list_rev : 'a t -> 'a list
  180 val Base.Pretty_printer.all : unit -> string list
  182 val Base.List.all_unit : unit t list -> unit t
  182 val Base.List.filter_opt : 'a option t -> 'a t
  182 val Base.List.transpose_exn : 'a t t -> 'a t t
  182 val Base.List.concat_no_order : 'a t t -> 'a t
  199 val Base.Set.to_list : ('a, _) t -> 'a list
  200 val Base.Hashtbl.data : (_, 'b) t -> 'b list
  200 val Base.Set.elements : ('a, _) t -> 'a list
  201 val Base.List.drop : 'a t -> int -> 'a t
  201 val Base.List.take : 'a t -> int -> 'a t
  202 val Base.String.split : t -> on:char -> t list
  204 val Base.List.append : 'a t -> 'a t -> 'a t
  204 val Base.Hashtbl.keys : ('a, _) t -> 'a key list
  208 val Base.List.rev_append : 'a t -> 'a t -> 'a t
  211 val Base.List.intersperse : 'a t -> sep:'a -> 'a t

Partial name search:
  $ sherlodoc search --print-cost "strin"
  147 type Base.string = String.t
  159 type Base.Export.string = String.t
  166 val Base.Sexp.of_string : unit
  167 type Base.String.t = string
  167 type Base.String.elt = char
  169 val Base.String.rev : t -> t
  171 mod Base.String
  171 mod Caml.String
  172 val Base.String.hash : t -> int
  172 val Base.Exn.to_string : t -> string
  172 val Base.Sys.max_string_length : int
  173 val Base.String.escaped : t -> t
  173 val Base.String.max_length : int
  174 val Base.String.(^) : t -> t -> t
  174 val Base.Float.to_string : t -> string
  175 mod Base.Stringable
  175 val Base.String.uppercase : t -> t
  176 type Base.String.Caseless.t = t
  176 val Base.String.capitalize : t -> t
  177 mod Caml.StringLabels
  177 val Base.String.append : t -> t -> t
  177 val Base.Exn.to_string_mach : t -> string
  177 val Base.Info.to_string_hum : t -> string
  177 val Base.Sign.to_string_hum : t -> string
  178 val Base.Info.to_string_mach : t -> string
  $ sherlodoc search --print-cost "tring"
  177 type Base.string = String.t
  182 type Base.String.t = string
  182 type Base.String.elt = char
  184 val Base.String.rev : t -> t
  186 mod Base.String
  186 mod Caml.String
  186 val Base.Sexp.of_string : unit
  187 val Base.String.hash : t -> int
  188 val Base.String.escaped : t -> t
  188 val Base.String.max_length : int
  189 val Base.String.(^) : t -> t -> t
  189 type Base.Export.string = String.t
  190 val Base.String.uppercase : t -> t
  191 type Base.String.Caseless.t = t
  191 val Base.String.capitalize : t -> t
  192 val Base.Exn.to_string : t -> string
  192 val Base.String.append : t -> t -> t
  194 val Base.String.equal : t -> t -> bool
  194 val Base.String.prefix : t -> int -> t
  194 val Base.String.suffix : t -> int -> t
  194 val Base.Float.to_string : t -> string
  195 val Base.String.compare : t -> t -> int
  195 mod Shadow_stdlib.String
  197 val Base.String.ascending : t -> t -> int
  197 val Base.String.split_lines : t -> t list
