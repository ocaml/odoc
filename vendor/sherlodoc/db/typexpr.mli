type t =
  | Arrow of t * t
  | Constr of string * t list
  | Tuple of t list
  | Poly of string
  | Any
  | Unhandled

val tuple : t list -> t
val size : t -> int
val show : t -> string
val equal : t -> t -> bool
val hash : t -> int
