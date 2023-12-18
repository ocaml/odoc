type t = private
  | Arrow of t * t
  | Constr of string * t list
  | Tuple of t list
  | Poly of string
  | Any
  | Unhandled

val arrow : t -> t -> t
val constr : string -> t list -> t
val tuple : t list -> t
val poly : string -> t
val any : t
val unhandled : t
val size : t -> int
val show : t -> string
