(** This module provides a way to get the first n elements of a very large set
    without computing the whole list of elements. *)

type 'a t

val print : ('a -> unit) -> 'a t -> unit
val to_seq : compare:('a -> 'a -> int) -> 'a t -> 'a Seq.t

(** Functions to build a succ tree *)

val empty : 'a t

val of_array : 'a array -> 'a t
(** Warning : only provide a sorted array, this is not checked !
    It has to be sorted according to the [compare] function that you will
    eventually pass to [to_seq]. *)

val inter : 'a t -> 'a t -> 'a t
val union : 'a t -> 'a t -> 'a t
val union_of_array : 'a t array -> 'a t

val union_of_list : 'a t list -> 'a t
(** [union_of_list] has better performance than [List.fold_left union empty]. *)

val inter_of_list : 'a t list -> 'a t
