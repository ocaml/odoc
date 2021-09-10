module type S = sig
  module M: sig
    type t
  end
  type t = M.t
end
module type T = S with type M.t := int

