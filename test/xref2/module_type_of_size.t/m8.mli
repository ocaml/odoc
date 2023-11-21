module type M = module type of M7 with module N = M1.T
module N : sig type t end
