(** Test case for include with type substitution referencing outer type.

    When the include's signature has a TypeSubstitution like [type reader := reader],
    the RHS [reader] refers to the outer type, not the substitution itself.
    This should not create a self-referential TypeSubstitution. *)

module type Read = sig
  type reader = unit

  include sig
    type reader := reader

    val bin_read_unit : reader
  end
end
