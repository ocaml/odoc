type foo

val unique_name : foo
val multiple_hit_1 : foo
val multiple_hit_2 : foo
val multiple_hit_3 : foo

type name_conflict = foo

val name_conflict : foo

module Nest : sig
  val nesting_priority : foo
end

val nesting_priority : foo

module Map : sig
  val to_list : foo
end

type 'a list

module List : sig
  type 'a t = 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val rev_map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
end

val foo : foo
(** this is not a list nor a map *)

type moo
type t

val value : moo
val consume : moo -> unit
val consume_2 : moo -> moo -> unit
val consume_2_other : moo -> t -> unit
val produce : unit -> moo
val produce_2' : unit -> unit -> moo

module type Modtype = sig
  val v_modtype : foo
end

module type S = sig end

module S_to_S1 : sig end

(**/**)

val hidden : foo

(**/**)

val poly_1 : 'a -> 'b -> 'c
val poly_2 : 'a -> 'b -> 'c -> 'a -> 'b -> 'c

type 'a boo

val poly_param : 'a boo

type extensible_type = ..
type extensible_type += MyExtension of moo
