module type M = module type of M10 with module N = M1.T
module N : sig type t end
