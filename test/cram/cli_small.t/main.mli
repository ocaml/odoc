
type 'a list

module List : sig
  type 'a t = 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t
end

