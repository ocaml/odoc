open Common

type 'a t =
  | Leaf of char list * 'a
  | Node of
      { leaf : 'a option
      ; children : 'a t Char.Map.t
      }

val empty : 'a t
val add : char list -> ('a option -> 'a) -> 'a t -> 'a t
val find : char list -> 'a t -> ('a t, [> `Stopped_at of int * 'a t ]) result
val fold_map : ('a -> 'a -> 'a) -> ('b -> 'a) -> 'b t -> 'a option
val map_leaf : f:('a -> 'b) -> 'a t -> 'b t
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
