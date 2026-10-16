type 'a t = Empty | Leaf of 'a
type 'a t_float = Empty_f | Leaf_f of 'a
type 'a u
type 'a v = 'a list

type 'a t := 'a t
type 'a u := 'a u
type 'a v := 'a v
val first : 'a t -> 'a u -> 'a v -> unit

type 'a t := 'a t_float
type 'a u := 'a t_float
type 'a v := 'a t_float
val first_float : 'a t -> 'a u -> 'a v -> unit
