(** A destructively-substituted module type referenced from a [with]-clause
    in a later module type definition. *)
module type Inner = sig
  module type X := sig type t end

  module type S = X with type t := int
end

include Inner
