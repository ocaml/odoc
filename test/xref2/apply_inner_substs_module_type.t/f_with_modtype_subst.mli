(** A destructively-substituted module type appearing not as a top-level
    [module type X := ...] item but as the substitution clause of a regular
    [with module type X := ...] expression. The replacement enters
    [module_type_replacement] via [fragmap]'s [sub_of_removed], not via
    [apply_inner_substs]. *)
module type T = sig
  module type X = sig type t end
  module type Y = X with type t := int
end

module M : T with module type X := sig type t end
