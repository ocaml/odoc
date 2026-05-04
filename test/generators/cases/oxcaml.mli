val f : int -> ('a . 'a -> 'a) -> unit
(** Polymorphic arguments require parentheses *)

val add : bool -> int -> int -> int [@@zero_alloc]
(** Zero allocation bindings have an extension attribute attached.
    See https://oxcaml.org/documentation/miscellaneous-extensions/zero_alloc_check/
 *)

val add_opt : bool -> int -> int -> int [@@zero_alloc opt]
(** Like [add] but with an [opt] attribute.
 *)

val add_strict : bool -> int -> int -> int [@@zero_alloc strict]
(** Like [add] but with a [strict] attribute.
 *)

val[@zero_alloc] f : int -> int
(** Alternative syntax for zero alloc annotation *)
