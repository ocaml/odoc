module type S = sig
  module X : sig type t end
  module type Y = module type of X
  module type Z = module type of struct include X end
end
module X1 : sig type t type u end
module type T = S with module X := X1
