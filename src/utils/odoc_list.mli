val concat_map_sep : sep:'a -> f:('b -> 'a list) -> 'b list -> 'a list
val split_at : f:('a -> bool) -> 'a list -> 'a list * 'a list
val skip_until : p:('a -> bool) -> 'a list -> 'a list

val last : 'a list -> 'a
(** @raise Failure if the list is empty. *)

val is_empty : 'a list -> bool
val find_map : ('a -> 'b option) -> 'a list -> 'b option
val concat_map : ('a -> 'b list) -> 'a list -> 'b list

include module type of List
