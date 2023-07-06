(** This module provides a way to get the first n elements of a very large set
    without computing the other elements. *)

type 'a t

val print : ('a -> unit) -> 'a t -> unit
val to_seq : compare:('a -> 'a -> int) -> 'a t -> 'a Seq.t

(** Functions to build a succ tree *)

type 'a builder

val finish : 'a builder -> 'a t
val all : 'a builder
val empty : 'a builder

val of_array : 'a array -> 'a builder
(** Warning : only provide a sorted array, this is not checked !
    It also has to be sorted according to the [compare] function that you will
    eventually pass to [to_seq] *)

val inter : 'a builder -> 'a builder -> 'a builder
val union : 'a builder -> 'a builder -> 'a builder
val union_of_list : 'a builder list -> 'a builder
