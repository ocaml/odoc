type 'a tree = { node : 'a; children : 'a forest }
and 'a forest = 'a tree list

val leaf : 'a -> 'a tree

module type S = sig
  type 'a t

  val fold_left : f:('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val iter : f:('a -> unit) -> 'a t -> unit
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val to_json : ('a -> Json.json) -> 'a t -> Json.json
end

include S with type 'a t = 'a tree

module Forest : sig
  include S with type 'a t = 'a forest

  val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
end
