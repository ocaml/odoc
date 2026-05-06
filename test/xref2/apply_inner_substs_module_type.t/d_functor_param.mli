(** A destructively-substituted module type referenced from a [with]-clause
    in a functor parameter constraint. *)
module type Inner = sig
  module type X := sig type t end

  module F : functor (M : X with type t := int) -> sig end
end

include Inner
