val f : int -> ('a . 'a -> 'a) -> unit
(** Polymorphic arguments require parentheses *)

(** Unboxed types have a trailing hash '#' *)

type pt = { x : int ; y : float32# }
type segment = { start : pt# ; stop : pt# }
