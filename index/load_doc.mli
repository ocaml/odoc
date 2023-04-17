module Types = Db.Types
module ModuleName = Odoc_model.Names.ModuleName

val copy : string -> string
val deep_copy : 't -> 't

module Cache_doc : sig
  module H : Hashtbl.S with type key = Html_types.li_content_fun Tyxml_html.elt

  val cache : Html_types.li_content_fun Tyxml_html.elt H.t
  val clear : unit -> unit
  val memo : H.key -> Html_types.li_content_fun Tyxml_html.elt
end

module Cache_name : sig
  module H : sig
    type key = string
    type !'a t

    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  val cache : string H.t
  val clear : unit -> unit
  val memo : H.key -> string
end

module Cache : sig
  module H : sig
    type key = string
    type !'a t

    val create : int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

  val cache : string H.t
  val clear : unit -> unit
  val memo : H.key -> string
end

val clear : unit -> unit
val type_size : Odoc_model.Lang.TypeExpr.t -> int
val rev_concat : 'a list list -> 'a list
val tails : 'a list -> 'a list list
val fullname : 'a -> 'b
val all_type_names : 'a -> string list list

val paths :
     prefix:string list
  -> sgn:Types.sgn
  -> Odoc_model.Lang.TypeExpr.t
  -> string list list

val type_paths :
     prefix:string list
  -> sgn:Types.sgn
  -> Odoc_model.Lang.TypeExpr.t
  -> string list list

val save_item :
     pkg:string * string
  -> path_list:char list
  -> path:'a list
  -> Odoc_model.Names.ValueName.t
  -> Odoc_model.Lang.TypeExpr.t
  -> 'b
  -> unit

val item :
     pkg:string * string
  -> path_list:char list
  -> path:string list
  -> Odoc_model.Lang.Signature.item
  -> unit

val items :
     pkg:string * string
  -> path_list:char list
  -> path:string list
  -> Odoc_model.Lang.Signature.item list
  -> unit

val module_items :
     pkg:string * string
  -> path_list:char list
  -> path:string list
  -> Odoc_model.Lang.Module.t
  -> unit

val module_type_expr :
     pkg:string * string
  -> path_list:char list
  -> path:string list
  -> Odoc_model.Lang.ModuleType.expr
  -> unit

val simple_expansion :
     pkg:string * string
  -> path_list:char list
  -> path:string list
  -> Odoc_model.Lang.ModuleType.simple_expansion
  -> unit

val module_items_ty :
     pkg:string * string
  -> path_list:char list
  -> path:string list
  -> Odoc_model.Lang.ModuleType.simple_expansion
  -> unit

module Resolver = Odoc_odoc.Resolver

val run : odoc_directory:string -> string * string -> unit
