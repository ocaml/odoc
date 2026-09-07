module type S_fc = sig
  type comparable_t

  val f : comparable_t -> comparable_t
end

val make : compare:('a -> 'a -> int) -> (module S_fc with type comparable_t = 'a)
