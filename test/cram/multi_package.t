  $ export ODOCLS=$(find ../docs/odoc/ -name '*.odocl' | sort)
  $ echo $ODOCLS | wc -w
  557
  $ export SHERLODOC_DB=db.bin
  $ export SHERLODOC_FORMAT=marshal
  $ sherlodoc index --index-docstring=false $ODOCLS > /dev/null
  $ sherlodoc search --print-cost --limit 100 "S_poly"
  150 sig Base.Map.S_poly
  150 sig Base.Set.S_poly
  154 sig Base.Hashtbl.S_poly
  198 type 'a Base.Hashtbl.S_poly.key = 'a
  207 type ('a, 'b) Base.Map.S_poly.t
  207 type 'elt Base.Set.S_poly.t
  209 type ('a, 'cmp) Base.Set.S_poly.set
  210 val Base.Set.S_poly.mem : 'a t -> 'a -> bool
  210 type ('a, 'b) Base.Map.S_poly.tree
  210 type 'elt Base.Set.S_poly.tree
  211 type ('a, 'b) Base.Hashtbl.S_poly.t
  211 mod Base.Set.S_poly.Named
  217 val Base.Hashtbl.S_poly.data : (_, 'b) t -> 'b list
  221 val Base.Hashtbl.S_poly.keys : ('a, _) t -> 'a key list
  224 type Base.Map.S_poly.comparator_witness
  224 type Base.Set.S_poly.comparator_witness
  227 val Base.Set.S_poly.map : ('a, _) set -> f:('a -> 'b) -> 'b t
  227 val Base.Hashtbl.S_poly.find_exn : ('a, 'b) t -> 'a key -> 'b
  228 val Base.Hashtbl.S_poly.choose_exn : ('a, 'b) t -> 'a key * 'b
  230 val Base.Hashtbl.S_poly.find : ('a, 'b) t -> 'a key -> 'b option
  233 val Base.Hashtbl.S_poly.choose : ('a, 'b) t -> ('a key * 'b) option
  233 val Base.Hashtbl.S_poly.to_alist : ('a, 'b) t -> ('a key * 'b) list
  233 mod Base.Map.S_poly.Make_applicative_traversals
  236 val Base.Hashtbl.S_poly.map : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t
  237 val Base.Hashtbl.S_poly.map_inplace : (_, 'b) t -> f:('b -> 'b) -> unit
  237 val Base.Hashtbl.S_poly.remove_multi : ('a, _ list) t -> 'a key -> unit
  239 val Base.Hashtbl.S_poly.set : ('a, 'b) t -> key:'a key -> data:'b -> unit
  239 val Base.Hashtbl.S_poly.find_multi : ('a, 'b list) t -> 'a key -> 'b list
  241 val Base.Hashtbl.S_poly.find_and_remove : ('a, 'b) t -> 'a key -> 'b option
  250 val Base.Hashtbl.S_poly.update : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> unit
  250 val Base.Hashtbl.S_poly.add_multi : ('a, 'b list) t -> key:'a key -> data:'b -> unit
  250 val Base.Hashtbl.S_poly.filter_map : ('a, 'b) t -> f:('b -> 'c option) -> ('a, 'c) t
  251 val Base.Hashtbl.S_poly.filter_map_inplace : (_, 'b) t -> f:('b -> 'b option) -> unit
  251 val Base.Hashtbl.S_poly.filter_keys_inplace : ('a, _) t -> f:('a key -> bool) -> unit
  252 val Base.Hashtbl.S_poly.equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  253 val Base.Hashtbl.S_poly.iteri : ('a, 'b) t -> f:(key:'a key -> data:'b -> unit) -> unit
  254 val Base.Hashtbl.S_poly.find_or_add : ('a, 'b) t -> 'a key -> default:(unit -> 'b) -> 'b
  255 val Base.Hashtbl.S_poly.add : ('a, 'b) t -> key:'a key -> data:'b -> [ `Ok | `Duplicate ]
  256 val Base.Hashtbl.S_poly.mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c) -> ('a, 'c) t
  257 val Base.Hashtbl.S_poly.change : ('a, 'b) t -> 'a key -> f:('b option -> 'b option) -> unit
  257 val Base.Hashtbl.S_poly.findi_or_add : ('a, 'b) t -> 'a key -> default:('a key -> 'b) -> 'b
  259 val Base.Hashtbl.S_poly.update_and_return : ('a, 'b) t -> 'a key -> f:('b option -> 'b) -> 'b
  260 val Base.Hashtbl.S_poly.partition_tf : ('a, 'b) t -> f:('b -> bool) -> ('a, 'b) t * ('a, 'b) t
  261 val Base.Hashtbl.S_poly.incr : ?by:int -> ?remove_if_zero:bool -> ('a, int) t -> 'a key -> unit
  269 val Base.Hashtbl.S_poly.choose_randomly_exn : ?random_state:Random.State.t -> ('a, 'b) t -> 'a key * 'b
  270 val Base.Hashtbl.S_poly.filter_mapi : ('a, 'b) t -> f:(key:'a key -> data:'b -> 'c option) -> ('a, 'c) t
  273 val Base.Hashtbl.S_poly.fold : ('a, 'b) t -> init:'acc -> f:(key:'a key -> data:'b -> 'acc -> 'acc) -> 'acc
  274 val Base.Hashtbl.S_poly.partition_map : ('a, 'b) t -> f:('b -> ('c, 'd) Either.t) -> ('a, 'c) t * ('a, 'd) t
  274 val Base.Hashtbl.S_poly.choose_randomly : ?random_state:Random.State.t -> ('a, 'b) t -> ('a key * 'b) option
  280 val Base.Hashtbl.S_poly.partitioni_tf : ('a, 'b) t -> f:(key:'a key -> data:'b -> bool) -> ('a, 'b) t * ('a, 'b) t
  294 val Base.Hashtbl.S_poly.find_and_call : ('a, 'b) t ->
    'a key ->
    if_found:('b -> 'c) ->
    if_not_found:('a key -> 'c) ->
    'c
  298 val Base.Set.S_poly.empty : 'a t
  298 val Base.Hashtbl.S_poly.partition_mapi : ('a, 'b) t ->
    f:(key:'a key -> data:'b -> ('c, 'd) Either.t) ->
    ('a, 'c) t * ('a, 'd) t
  303 val Base.Map.S_poly.empty : ('k, _) t
  305 val Base.Set.S_poly.length : _ t -> int
  308 val Base.Set.S_poly.is_empty : _ t -> bool
  308 val Base.Set.S_poly.singleton : 'a -> 'a t
  309 val Base.Set.S_poly.choose_exn : 'a t -> 'a
  310 val Base.Set.S_poly.add : 'a t -> 'a -> 'a t
  310 val Base.Map.S_poly.length : (_, _) t -> int
  310 val Base.Set.S_poly.max_elt_exn : 'a t -> 'a
  310 val Base.Set.S_poly.min_elt_exn : 'a t -> 'a
  311 val Base.Set.S_poly.of_list : 'a list -> 'a t
  311 val Base.Set.S_poly.of_tree : 'a tree -> 'a t
  311 val Base.Set.S_poly.to_list : 'a t -> 'a list
  311 val Base.Set.S_poly.to_tree : 'a t -> 'a tree
  311 val Base.Set.S_poly.invariants : 'a t -> bool
  312 val Base.Set.S_poly.choose : 'a t -> 'a option
  312 val Base.Set.S_poly.elements : 'a t -> 'a list
  312 val Base.Hashtbl.S_poly.merge_into : src:('k, 'a) t ->
    dst:('k, 'b) t ->
    f:(key:'k key -> 'a -> 'b option -> 'b Merge_into_action.t) ->
    unit
  313 val Base.Map.S_poly.data : (_, 'v) t -> 'v list
  313 val Base.Map.S_poly.keys : ('k, _) t -> 'k list
  313 val Base.Set.S_poly.diff : 'a t -> 'a t -> 'a t
  313 val Base.Set.S_poly.remove : 'a t -> 'a -> 'a t
  313 val Base.Set.S_poly.max_elt : 'a t -> 'a option
  313 val Base.Set.S_poly.min_elt : 'a t -> 'a option
  313 val Base.Map.S_poly.is_empty : (_, _) t -> bool
  313 val Base.Set.S_poly.of_array : 'a array -> 'a t
  313 val Base.Set.S_poly.to_array : 'a t -> 'a array
  314 val Base.Set.S_poly.equal : 'a t -> 'a t -> bool
  314 val Base.Set.S_poly.inter : 'a t -> 'a t -> 'a t
  314 val Base.Set.S_poly.union : 'a t -> 'a t -> 'a t
  314 val Base.Hashtbl.S_poly.clear : (_, _) t -> unit
  314 val Base.Hashtbl.S_poly.length : (_, _) t -> int
  314 val Base.Hashtbl.S_poly.hashable : 'a Hashable.t
  315 val Base.Map.S_poly.mem : ('k, _) t -> 'k -> bool
  316 val Base.Set.S_poly.nth : 'a t -> int -> 'a option
  316 val Base.Set.S_poly.union_list : 'a t list -> 'a t
  317 val Base.Map.S_poly.invariants : ('k, 'v) t -> bool
  317 val Base.Hashtbl.S_poly.is_empty : (_, _) t -> bool
  317 val Base.Hashtbl.S_poly.find_and_call1 : ('a, 'b) t ->
    'a key ->
    a:'d ->
    if_found:('b -> 'd -> 'c) ->
    if_not_found:('a key -> 'd -> 'c) ->
    'c
  319 val Base.Map.S_poly.find_exn : ('k, 'v) t -> 'k -> 'v
  320 val Base.Map.S_poly.singleton : 'k -> 'v -> ('k, 'v) t
  320 val Base.Set.S_poly.remove_index : 'a t -> int -> 'a t
  321 val Base.Hashtbl.S_poly.copy : ('a, 'b) t -> ('a, 'b) t
  321 val Base.Map.S_poly.max_elt_exn : ('k, 'v) t -> 'k * 'v
  321 val Base.Map.S_poly.min_elt_exn : ('k, 'v) t -> 'k * 'v
  321 val Base.Set.S_poly.of_sequence : 'a Sequence.t -> 'a t
  321 val Base.Set.S_poly.are_disjoint : 'a t -> 'a t -> bool
  322 val Base.Set.S_poly.compare_direct : 'a t -> 'a t -> int
  $ sherlodoc search --print-cost --no-rhs "group b"
  127 val Str.group_beginning
  170 val Stdlib.Seq.group
  176 field Signature_group.in_place_patch.replace_by
  181 val Base.Set.group_by
  205 val Base.List.group
  212 val Base.Sequence.group
  220 field UnixLabels.group_entry.gr_gid
  224 field UnixLabels.group_entry.gr_name
  225 val Base.List.sort_and_group
  226 field UnixLabels.group_entry.gr_passwd
  228 val Base.List.groupi
  229 field UnixLabels.group_entry.gr_mem
  235 val Base.List.Assoc.group
  255 val Base.List.Assoc.sort_and_group
  275 val UnixLabels.getgroups
  275 val UnixLabels.setgroups
  275 val Base.Set.Poly.group_by
  280 val UnixLabels.initgroups
  297 type UnixLabels.group_entry
  303 val Base.Set.Using_comparator.group_by
  313 val Base.Set.Using_comparator.Tree.group_by
  323 val Base.Hashtbl.group
  377 val Base.Set.S_poly.group_by
  412 val Base.Set.Accessors_generic.group_by
  423 val Base.Hashtbl.Poly.group
  $ sherlodoc search --no-rhs "group by"
  field Signature_group.in_place_patch.replace_by
  val Base.Set.group_by
  val Base.Set.Poly.group_by
  val Base.Set.Using_comparator.group_by
  val Base.Set.Using_comparator.Tree.group_by
  val Base.Set.S_poly.group_by
  val Base.Set.Accessors_generic.group_by
  val Base.Set.Creators_and_accessors_generic.group_by
  $ sherlodoc search --print-cost "map2"
  73 val Stdlib.Seq.map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  83 val Stdlib.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  86 val Stdlib.Float.Array.map2 : (float -> float -> float) -> t -> t -> t
  87 val Stdlib.Array.map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  91 val Stdlib.ListLabels.map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  94 val Stdlib.Float.ArrayLabels.map2 : f:(float -> float -> float) -> t -> t -> t
  95 val Stdlib.ArrayLabels.map2 : f:('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
  97 val Stdlib.List.rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  105 val Stdlib.ListLabels.rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  127 mod Base.Applicative.Make_using_map2
  128 mod Base.Applicative.Make2_using_map2
  128 mod Base.Applicative.Make3_using_map2
  138 mod Base.Applicative.Make_using_map2_local
  139 mod Base.Applicative.Make2_using_map2_local
  139 mod Base.Applicative.Make3_using_map2_local
  142 val Base.Uniform_array.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  147 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  150 mod Base.Applicative.Make_using_map2.Applicative_infix
  151 mod Base.Applicative.Make2_using_map2.Applicative_infix
  151 mod Base.Applicative.Make3_using_map2.Applicative_infix
  153 val Misc.Stdlib.List.map2_prefix : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t * 'b t
  155 val Base.Applicative.Make_using_map2.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  161 mod Base.Applicative.Make_using_map2_local.Applicative_infix
  162 mod Base.Applicative.Make2_using_map2_local.Applicative_infix
  162 mod Base.Applicative.Make3_using_map2_local.Applicative_infix

  $ sherlodoc search --print-cost --static-sort "List map2"
  78 val Stdlib.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  82 val Stdlib.List.rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  86 val Stdlib.ListLabels.map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  90 val Stdlib.ListLabels.rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  127 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  143 val Misc.Stdlib.List.map2_prefix : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t * 'b t
  223 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  240 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  242 val Base.List.Cartesian_product.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  244 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

  $ sherlodoc search --print-cost "List map2"
  88 val Stdlib.List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  102 val Stdlib.List.rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  111 val Stdlib.ListLabels.map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  125 val Stdlib.ListLabels.rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  152 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  158 val Misc.Stdlib.List.map2_prefix : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t * 'b t
  238 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  250 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  252 val Base.List.Cartesian_product.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  264 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

  $ sherlodoc search --no-rhs "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group
  $ sherlodoc search --print-cost "list"
  56 mod Stdlib.List
  58 cons Stdlib.List.t.[] : 'a t
  60 val Stdlib.List.hd : 'a list -> 'a
  64 val Stdlib.Stream.of_list : 'a list -> 'a t
  65 val Stdlib.List.tl : 'a list -> 'a list
  65 val Stdlib.List.length : 'a list -> int
  66 val Stdlib.List.rev : 'a list -> 'a list
  67 val Stdlib.Array.of_list : 'a list -> 'a array
  67 val Stdlib.Array.to_list : 'a array -> 'a list
  68 val Stdlib.List.nth : 'a list -> int -> 'a
  69 val Stdlib.List.mem : 'a -> 'a list -> bool
  69 val Stdlib.Option.to_list : 'a option -> 'a list
  69 val Stdlib.Set.Make.of_list : elt list -> t
  70 val Stdlib.List.memq : 'a -> 'a list -> bool
  70 val Stdlib.List.of_seq : 'a Seq.t -> 'a list
  70 val Stdlib.List.to_seq : 'a list -> 'a Seq.t
  71 mod Stdlib.StdLabels.List
  73 val Stdlib.List.cons : 'a -> 'a list -> 'a list
  73 val Stdlib.ArrayLabels.of_list : 'a list -> 'a array
  74 cons Stdlib.List.t.:: : 'a * 'a list -> 'a t
  74 val Stdlib.List.concat : 'a list list -> 'a list
  75 val Stdlib.List.assq : 'a -> ('a * 'b) list -> 'b
  75 val Stdlib.List.flatten : 'a list list -> 'a list
  76 val Stdlib.List.assoc : 'a -> ('a * 'b) list -> 'b
  77 mod Stdlib.ListLabels
  $ sherlodoc search --print-cost ": list"
  43 cons Stdlib.List.t.[] : 'a t
  54 cons Stdlib.ListLabels.t.[] : 'a t
  70 val Stdlib.List.tl : 'a list -> 'a list
  71 val Stdlib.List.rev : 'a list -> 'a list
  75 val Stdlib.List.of_seq : 'a Seq.t -> 'a list
  76 val Stdlib.ListLabels.tl : 'a list -> 'a list
  77 val Stdlib.Array.to_list : 'a array -> 'a list
  77 val Stdlib.ListLabels.rev : 'a list -> 'a list
  79 val Stdlib.List.concat : 'a list list -> 'a list
  79 val Stdlib.Option.to_list : 'a option -> 'a list
  80 val Stdlib.List.flatten : 'a list list -> 'a list
  80 val Stdlib.Set.Make.elements : t -> elt list
  81 val Stdlib.ListLabels.of_seq : 'a Seq.t -> 'a list
  83 val Stdlib.ArrayLabels.to_list : 'a array -> 'a list
  84 val Stdlib.Float.Array.to_list : t -> float list
  85 val Stdlib.ListLabels.concat : 'a list list -> 'a list
  86 val Stdlib.ListLabels.flatten : 'a list list -> 'a list
  90 val Stdlib.Float.ArrayLabels.to_list : t -> float list
  97 val Stdlib.(@) : 'a list -> 'a list -> 'a list
  103 val Stdlib.List.cons : 'a -> 'a list -> 'a list
  104 val Stdlib.Stream.npeek : int -> 'a t -> 'a list
  109 cons Stdlib.List.t.:: : 'a * 'a list -> 'a t
  109 val Stdlib.ListLabels.cons : 'a -> 'a list -> 'a list
  110 val Stdlib.List.append : 'a list -> 'a list -> 'a list
  110 val Stdlib.Result.to_list : ('a, 'e) result -> 'a list

Partial name search:
  $ sherlodoc search --print-cost "strin"
  61 val Stdlib.string_of_int : int -> string
  63 val Stdlib.string_of_bool : bool -> string
  64 val Stdlib.Digest.string : string -> t
  65 val Stdlib.string_of_float : float -> string
  69 type Stdlib.String.t = string
  71 val Stdlib.prerr_string : string -> unit
  71 val Stdlib.print_string : string -> unit
  71 val Stdlib.int_of_string : string -> int
  73 mod Stdlib.String
  73 val Stdlib.String.empty : string
  73 val Stdlib.bool_of_string : string -> bool
  74 val Stdlib.Sys.max_string_length : int
  75 type Stdlib.StringLabels.t = string
  75 val Stdlib.Unit.to_string : t -> string
  75 val Stdlib.float_of_string : string -> float
  76 val Stdlib.Int.to_string : int -> string
  78 val Stdlib.Bool.to_string : bool -> string
  79 mod Stdlib.StringLabels
  79 val Stdlib.StringLabels.empty : string
  80 val Stdlib.String.create : int -> bytes
  80 val Stdlib.Bytes.of_string : string -> bytes
  80 val Stdlib.Bytes.to_string : bytes -> string
  80 val Stdlib.Float.of_string : string -> float
  80 val Stdlib.Float.to_string : float -> string
  80 val Stdlib.Int32.to_string : int32 -> string
  $ sherlodoc search --print-cost "base strin"
  112 type Base.string = String.t
  124 type Base.Export.string = String.t
  131 val Base.Sexp.of_string : unit
  132 type Base.String.t = string
  132 type Base.String.elt = char
  134 val Base.String.rev : t -> t
  136 mod Base.String
  137 val Base.String.hash : t -> int
  137 val Base.Exn.to_string : t -> string
  137 val Base.Sys.max_string_length : int
  138 val Base.String.escaped : t -> t
  138 val Base.String.max_length : int
  139 val Base.String.(^) : t -> t -> t
  139 val Base.Float.to_string : t -> string
  140 mod Base.Stringable
  140 val Base.String.uppercase : t -> t
  141 type Base.String.Caseless.t = t
  141 val Base.String.capitalize : t -> t
  142 mod Base.StringLabels
  142 val Base.String.append : t -> t -> t
  142 val Base.Exn.to_string_mach : t -> string
  142 val Base.Info.to_string_hum : t -> string
  142 val Base.Sign.to_string_hum : t -> string
  143 val Base.Error.to_string_hum : t -> string
  143 val Base.Info.to_string_mach : t -> string

  $ sherlodoc search --print-cost "tring"
  84 type Stdlib.String.t = string
  88 mod Stdlib.String
  88 val Stdlib.String.empty : string
  91 val Stdlib.prerr_string : string -> unit
  91 val Stdlib.print_string : string -> unit
  91 val Stdlib.int_of_string : string -> int
  93 val Stdlib.bool_of_string : string -> bool
  94 val Stdlib.Digest.string : string -> t
  95 val Stdlib.String.create : int -> bytes
  95 val Stdlib.Unit.to_string : t -> string
  95 val Stdlib.float_of_string : string -> float
  96 val Stdlib.String.equal : t -> t -> bool
  96 val Stdlib.Int.to_string : int -> string
  96 val Stdlib.String.length : string -> int
  96 val Stdlib.string_of_int : int -> string
  97 val Stdlib.String.copy : string -> string
  97 val Stdlib.String.trim : string -> string
  97 val Stdlib.String.compare : t -> t -> int
  98 val Stdlib.String.of_seq : char Seq.t -> t
  98 val Stdlib.String.to_seq : t -> char Seq.t
  98 val Stdlib.Bool.to_string : bool -> string
  98 val Stdlib.string_of_bool : bool -> string
  99 val Stdlib.Sys.max_string_length : int
  100 val Stdlib.String.escaped : string -> string
  100 val Stdlib.Int32.to_string : int32 -> string
  $ sherlodoc search --print-cost "base tring"
  142 type Base.string = String.t
  147 type Base.String.t = string
  147 type Base.String.elt = char
  149 val Base.String.rev : t -> t
  151 mod Base.String
  151 val Base.Sexp.of_string : unit
  152 val Base.String.hash : t -> int
  153 val Base.String.escaped : t -> t
  153 val Base.String.max_length : int
  154 val Base.String.(^) : t -> t -> t
  154 type Base.Export.string = String.t
  155 val Base.String.uppercase : t -> t
  156 type Base.String.Caseless.t = t
  156 val Base.String.capitalize : t -> t
  157 val Base.Exn.to_string : t -> string
  157 val Base.String.append : t -> t -> t
  159 val Base.String.equal : t -> t -> bool
  159 val Base.String.prefix : t -> int -> t
  159 val Base.String.suffix : t -> int -> t
  159 val Base.Float.to_string : t -> string
  160 val Base.String.compare : t -> t -> int
  162 val Base.String.ascending : t -> t -> int
  162 val Base.String.split_lines : t -> t list
  162 val Base.Sys.max_string_length : int
  164 val Base.String.common_prefix : t list -> t

