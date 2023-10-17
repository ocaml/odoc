(** [Occ] stands for occurences. It associate sets of elements to the number
    of time members of the set occurs. 
    The list [[a, a, b, b, c]] would correspond to [[(2, [a; b]); (1, [c]) ]].
    It is used or type search : you want to be able to return every function
    that takes two ints as an argument. Without this datastrucure, we would only 
    to search for functions that take ints, without specifying the amount. *)

type t
type elt = int * Elt.t

val find : int -> t -> Elt.t array option
val fold : (int -> Elt.t array -> 'a -> 'a) -> t -> 'a -> 'a
val is_empty : t -> bool
val equal_elt : elt -> elt -> bool
val of_list : elt list -> t
