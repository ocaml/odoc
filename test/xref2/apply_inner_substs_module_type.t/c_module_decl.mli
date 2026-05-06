(** A destructively-substituted module type referenced from a [with]-clause
    in a later module declaration. *)
module type Inner = sig
  module type X := sig type t end

  module M : X with type t := int
end

include Inner
