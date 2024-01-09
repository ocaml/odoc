type elt = Db.Entry.t
type t

val minimum : t -> elt option
val of_automata : Db.String_automata.t -> t
val of_sorted_array : elt array option -> t
val pop_lt : elt -> t -> t
val pop_lte : elt -> t -> t
