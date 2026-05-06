(** A destructively-substituted module type referenced from a [with]-clause
    nested inside a sub-module type. *)
module type Inner = sig
  module type X := sig type t end

  module type S = sig
    module N : X with type t := int
  end
end

include Inner
