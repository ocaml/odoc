type t 
type elt = int * Elt.t
val find : int -> t -> Elt.t array option
val fold : (int -> Elt.t array -> 'a -> 'a) -> t -> 'a -> 'a
val is_empty : t -> bool
val equal_elt : elt -> elt -> bool
val of_list : elt list -> t
