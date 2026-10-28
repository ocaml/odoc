module type S = sig
  type t
end

module F (A : S) : sig
  module type R = sig
    module E = Ext

    type u = A.t
  end
end

module type T

module type U = sig
  module O : T
end

module G (A : U) : sig
  module type R = sig
    module E = Ext

    module O : module type of A.O
  end
end
