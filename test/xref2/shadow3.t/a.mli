module type B = sig
  module A : sig
    type t
  end
end

include B

module type B1 = sig
  module A : sig
    include module type of struct include A end
    type a
  end
end

include B1

