module type M = module type of M6 with module N = M1.T
module N : sig type t end
