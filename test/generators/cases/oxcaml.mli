val f : int -> ('a . 'a -> 'a) -> unit
(** Polymorphic arguments require parentheses *)

val add : bool -> int -> int -> int [@@zero_alloc]
(** Zero allocation bindings have an extension attribute attached.
    See https://oxcaml.org/documentation/miscellaneous-extensions/zero_alloc_check/
 *)
