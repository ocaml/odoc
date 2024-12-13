
type 'a list

module List : sig
  type 'a t = 'a list

  val map : ('a -> 'b) -> 'a t -> 'b t

  val empty : 'a t * 'b t


end

type ('a, 'b) result

val ok: 'a -> ('a, 'b) result

val ok_zero : (int, 'a) result