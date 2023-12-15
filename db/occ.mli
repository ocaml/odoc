(** [Occ] stands for occurences. It associate sets of elements to the number of
  time members of the set occurs.

The list [[a, a, b, b, c]] would correspond to [[(2, [a; b]); (1, [c]) ]]. It is
used or type search : you want to be able to return every function that takes
two ints as an argument. Without this datastrucure, we would only to search for
functions that take ints, without specifying the amount.

This datastructure is used at the leafs of the suffix tree : so when doing type
search, we first perform a type search ignoring occurences, and afterwards
filter the results according to them. *)

type t
type elt = int * Entry.t

val find : int -> t -> Entry.t array option
val fold : (int -> Entry.t array -> 'a -> 'a) -> t -> 'a -> 'a
val is_empty : t -> bool
val equal_elt : elt -> elt -> bool
val of_list : elt list -> t
