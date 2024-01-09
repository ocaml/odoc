(** This module provides a way to get the first n elements of a very large set
    without computing the whole list of elements. *)

type t

val to_seq : t -> Db.Entry.t Seq.t
val empty : t
val of_automata : Db.String_automata.t -> t
val inter : t -> t -> t
val union : t -> t -> t
val inter_of_list : t list -> t
val of_array : Db.Entry.t array -> t
