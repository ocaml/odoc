module type Value = sig
  val print : unit
end

module type Set = sig
  type t

  val empty : unit

  module Element : Value
end

module type Function = sig
  module Domain : Set

  module Point : sig
    include module type of Domain.Element
  end
end

module Inverse (F : Function with type Domain.t = unit) : sig end
