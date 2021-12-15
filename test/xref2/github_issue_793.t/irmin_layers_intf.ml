module Foo = struct end

module type Foo_T = sig end

module type A = sig
  type unrelated

  type conflicting_type := [ `Contents | `Node ]

  module Conflicting_module := Foo

  module type Conflicting_module_type := Foo_T
end

module type B = sig
  include A

  type conflicting_type = Foo

  module Conflicting_module = Foo

  module type Conflicting_module_type = Foo_T
end

module type C = sig
  include B
end

module type Maker = sig
  type hash

  module Make (Schema : sig end) : C with type unrelated = int
end
