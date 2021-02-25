module Bar = struct end

module type S = sig
  include A_intf.S

  module Foo : sig end
end

module type B = sig
  module type S = S

  include S
end
