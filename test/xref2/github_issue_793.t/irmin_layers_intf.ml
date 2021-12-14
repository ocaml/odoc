module Irmin = struct
  module type S_generic_key = sig
    type repo

    type kinded_key := [ `Contents | `Node ]

    val f : kinded_key
  end

  module type S = sig
    include S_generic_key
  end

  module Generic_key = struct
    module type S = S_generic_key
  end
end

module type Generic_key = sig
  include Irmin.Generic_key.S

  type kinded_key = Foo
end

module type S = sig
  include Generic_key
end

module type Maker = sig
  type hash

  module Make (Schema : sig end) : S with type repo = int
end
