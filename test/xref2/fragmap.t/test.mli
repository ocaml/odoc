
module type S = sig
  module N : sig
   type t
  end

  module N2 = N
end

module N3 : sig
  type t = int
end

module type S2 = S with module N := N3

