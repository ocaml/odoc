module type S = sig
  module Foo : sig end
end

module type A = sig
  module type S = S
end
