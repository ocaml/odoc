(** A destructively-substituted module type referenced from a [with]-clause
    in a later [include]. *)
module type Inner = sig
  module type X := sig type t end

  include X with type t := int
end

include Inner
