module type M = module type of M12 with module N = M1.T
module N : sig type t end
