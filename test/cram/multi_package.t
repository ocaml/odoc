  $ ODOCLS=$(find ../docs/odoc/ -name '*.odocl' | sort)
  $ echo "$ODOCLS" | awk 'END { print NR }'
  142
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
  181 val Base.Set.group_by
  205 val Base.List.group
  212 val Base.Sequence.group
  225 val Base.List.sort_and_group
  228 val Base.List.groupi
  235 val Base.List.Assoc.group
  255 val Base.List.Assoc.sort_and_group
  275 val Base.Set.Poly.group_by
  303 val Base.Set.Using_comparator.group_by
  313 val Base.Set.Using_comparator.Tree.group_by
  323 val Base.Hashtbl.group
  377 val Base.Set.S_poly.group_by
  412 val Base.Set.Accessors_generic.group_by
  423 val Base.Hashtbl.Poly.group
  425 val Base.Set.Creators_and_accessors_generic.group_by
  430 val Base.Hashtbl.Creators.group
  437 val Base.Hashtbl.Creators.group
  449 val Base.Hashtbl.S_without_submodules.group
  525 val Base.Hashtbl.S_poly.group
  $ sherlodoc search --no-rhs "group by"
  val Base.Set.group_by
  val Base.Set.Poly.group_by
  val Base.Set.Using_comparator.group_by
  val Base.Set.Using_comparator.Tree.group_by
  val Base.Set.S_poly.group_by
  val Base.Set.Accessors_generic.group_by
  val Base.Set.Creators_and_accessors_generic.group_by
  $ sherlodoc search --print-cost "map2"
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
  155 val Base.Applicative.Make_using_map2.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  161 mod Base.Applicative.Make_using_map2_local.Applicative_infix
  162 mod Base.Applicative.Make2_using_map2_local.Applicative_infix
  162 mod Base.Applicative.Make3_using_map2_local.Applicative_infix
  166 val Base.Applicative.Make_using_map2_local.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  178 sig Base.Applicative.Basic_using_map2
  178 val Base.Applicative.Make_using_map2.Applicative_infix.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  179 sig Base.Applicative.Basic2_using_map2
  179 sig Base.Applicative.Basic3_using_map2
  189 sig Base.Applicative.Basic_using_map2_local
  189 val Base.Applicative.Make_using_map2_local.Applicative_infix.(<*>) : ('a -> 'b) X.t -> 'a X.t -> 'b X.t
  190 sig Base.Applicative.Basic2_using_map2_local
  190 sig Base.Applicative.Basic3_using_map2_local
  226 val Base.Option.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  $ sherlodoc search --print-cost --static-sort "List map2"
  127 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  223 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  240 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  242 val Base.List.Cartesian_product.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  244 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

  $ sherlodoc search --print-cost "List map2"
  152 val Base.List.rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  238 val Base.List.map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  250 val Base.List.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t
  252 val Base.List.Cartesian_product.map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  264 val Base.List.rev_map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t Or_unequal_lengths.t

  $ sherlodoc search --no-rhs "Base.Hashtbl.S_without_submodules.group"
  val Base.Hashtbl.S_without_submodules.group
  $ sherlodoc search --print-cost "list"
  81 type 'a Base.list = 'a List.t
  93 type 'a Base.Export.list = 'a List.t
  101 type 'a Base.List.t = 'a list
  104 mod Base.List
  104 mod Caml.List
  108 val Base.List.rev : 'a t -> 'a t
  109 val Base.List.hd_exn : 'a t -> 'a
  109 val Base.List.return : 'a -> 'a t
  110 val Base.Bytes.to_list : t -> char list
  111 val Base.List.join : 'a t t -> 'a t
  111 val Base.List.tl_exn : 'a t -> 'a t
  111 val Base.Queue.of_list : 'a list -> 'a t
  111 val Base.Stack.of_list : 'a list -> 'a t
  113 val Base.List.concat : 'a t t -> 'a t
  113 mod Shadow_stdlib.List
  114 val Base.List.last : 'a t -> 'a option
  114 val Base.Set.to_list : ('a, _) t -> 'a list
  115 mod Base.List.Assoc
  115 mod Base.List.Infix
  115 cons Base.Sexp.t.List : t list -> t
  115 val Base.List.ignore_m : 'a t -> unit t
  115 val Base.Bytes.of_char_list : char list -> t
  116 val Base.List.drop : 'a t -> int -> 'a t
  116 val Base.List.take : 'a t -> int -> 'a t
  117 val Base.List.nth_exn : 'a t -> int -> 'a
  $ sherlodoc search --print-cost ": list"
  118 val Base.List.rev : 'a t -> 'a t
  119 val Base.List.return : 'a -> 'a t
  120 val Base.Bytes.to_list : t -> char list
  121 val Base.List.join : 'a t t -> 'a t
  121 val Base.List.tl_exn : 'a t -> 'a t
  122 val Base.String.split_lines : t -> t list
  123 val Base.List.concat : 'a t t -> 'a t
  125 val Base.List.ignore_m : 'a t -> unit t
  125 val Base.String.to_list_rev : t -> char list
  128 val Base.Sequence.to_list_rev : 'a t -> 'a list
  130 val Base.Pretty_printer.all : unit -> string list
  132 val Base.List.all_unit : unit t list -> unit t
  132 val Base.List.filter_opt : 'a option t -> 'a t
  132 val Base.List.transpose_exn : 'a t t -> 'a t t
  132 val Base.List.concat_no_order : 'a t t -> 'a t
  149 val Base.Set.to_list : ('a, _) t -> 'a list
  150 val Base.Hashtbl.data : (_, 'b) t -> 'b list
  150 val Base.Set.elements : ('a, _) t -> 'a list
  151 val Base.List.drop : 'a t -> int -> 'a t
  151 val Base.List.take : 'a t -> int -> 'a t
  152 val Base.String.split : t -> on:char -> t list
  154 val Base.List.append : 'a t -> 'a t -> 'a t
  154 val Base.Hashtbl.keys : ('a, _) t -> 'a key list
  158 val Base.List.rev_append : 'a t -> 'a t -> 'a t
  161 val Base.List.intersperse : 'a t -> sep:'a -> 'a t

Partial name search:
  $ sherlodoc search --print-cost "strin"
  97 type Base.string = String.t
  109 type Base.Export.string = String.t
  116 val Base.Sexp.of_string : unit
  117 type Base.String.t = string
  117 type Base.String.elt = char
  119 val Base.String.rev : t -> t
  121 mod Base.String
  121 mod Caml.String
  122 val Base.String.hash : t -> int
  122 val Base.Exn.to_string : t -> string
  122 val Base.Sys.max_string_length : int
  123 val Base.String.escaped : t -> t
  123 val Base.String.max_length : int
  124 val Base.String.(^) : t -> t -> t
  124 val Base.Float.to_string : t -> string
  125 mod Base.Stringable
  125 val Base.String.uppercase : t -> t
  126 type Base.String.Caseless.t = t
  126 val Base.String.capitalize : t -> t
  127 mod Base.StringLabels
  127 mod Caml.StringLabels
  127 val Base.String.append : t -> t -> t
  127 val Base.Exn.to_string_mach : t -> string
  127 val Base.Info.to_string_hum : t -> string
  127 val Base.Sign.to_string_hum : t -> string
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
  127 type Base.string = String.t
  132 type Base.String.t = string
  132 type Base.String.elt = char
  134 val Base.String.rev : t -> t
  136 mod Base.String
  136 mod Caml.String
  136 val Base.Sexp.of_string : unit
  137 val Base.String.hash : t -> int
  138 val Base.String.escaped : t -> t
  138 val Base.String.max_length : int
  139 val Base.String.(^) : t -> t -> t
  139 type Base.Export.string = String.t
  140 val Base.String.uppercase : t -> t
  141 type Base.String.Caseless.t = t
  141 val Base.String.capitalize : t -> t
  142 val Base.Exn.to_string : t -> string
  142 val Base.String.append : t -> t -> t
  144 val Base.String.equal : t -> t -> bool
  144 val Base.String.prefix : t -> int -> t
  144 val Base.String.suffix : t -> int -> t
  144 val Base.Float.to_string : t -> string
  145 val Base.String.compare : t -> t -> int
  145 mod Shadow_stdlib.String
  147 val Base.String.ascending : t -> t -> int
  147 val Base.String.split_lines : t -> t list
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

