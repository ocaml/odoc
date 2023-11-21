module type M = module type of M1 with module N = M1.T
module N : sig type t end
