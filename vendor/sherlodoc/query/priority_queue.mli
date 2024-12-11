type elt = Db.Entry.t
type t

val minimum : t -> elt option
val of_automata : Db.String_automata.t -> t
val of_sorted_array : elt array -> t
val of_list : t list -> t
val pop_lt : elt -> t -> t
val pop_lte : elt -> t -> t
val size : t -> int
