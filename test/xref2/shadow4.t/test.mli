module type S = sig
  type t = int
end

module type T = sig
  include S

  type v = t

  type t = float
end

