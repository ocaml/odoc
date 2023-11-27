module Foo (X : sig
  val foo : int
end) =
  String

module Bar = struct
  type t
end

module Foo' (X : sig
  val foo : int
end) =
  Bar
