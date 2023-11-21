module type M = module type of M11 with module N = M1.T
module N : sig type t end
