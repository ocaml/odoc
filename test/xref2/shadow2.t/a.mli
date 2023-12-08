module type B1 = sig
  module A : sig
    type t
  end
end

include B1

module type B2 = sig
  module A : sig
    include module type of struct include A end
    type u
  end
end

include B2

module type B3 = sig
  module A : sig
    include module type of struct include A end
    type v
  end
end

include B3

