(** [Occ] stands for occurences. It associate sets of elements to the number of
    time members of the set occurs.

    The list [[a, a, b, b, c]] would correspond to [[(2, [a; b]); (1, [c]) ]]. It is
    used or type search : you want to be able to return every function that takes
    two ints as an argument. Without this datastrucure, we would only be able to
    search for functions that take ints, without specifying the amount.

    This datastructure is used at the leafs of the suffix tree : so when doing type
    search, we first perform a type search ignoring occurences, and afterwards
    filter the results according to them.

    I will give an example bellow, it is probably better to read {!Type_polarities}
    first to understand it completely.

    If you have the following entries :

    {[
      val a : string -> int
      val b : string -> string -> int
      val c : string -> string -> int * int
      val d : string * string -> float -> int * int
    ]}

    Their polarities will be :

    {[
      val a : {(-string, 1); (+int, 1)}
      val b : {(-string, 2); (+int, 1)}
      val c : {(-string, 2); (+int, 2)}
      val d : {(-string, 2); (+int, 2); (-float, 1)}
    ]}

    We can combine them into a database that will look like this :

    {[
      +int ->
        { 1 -> {a; b}
          2 -> {c; d}
        }
      -string ->
        { 1 -> {a}
          2 -> {b; c; d}
        }
      -float ->
        { 1 -> {d}
        }
    ]}

    If there is a query for type [string -> string -> (int * int)], the polarities
    of the query are [(-string, 2)], [(+int, 2)].

    The entries of [(-string, 2)] are [{b; c; d}], and the entries of [(+int, 2)]
    are [{c; d}]. The intersection of the two is [{c; d}]. *)

type t
type elt = int * Entry.t

val find : int -> t -> Entry.t array option
val fold : (int -> Entry.t array -> 'a -> 'a) -> t -> 'a -> 'a
val is_empty : t -> bool
val equal_elt : elt -> elt -> bool
val of_list : elt list -> t
