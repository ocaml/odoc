module type Has_t = sig
  type t = int

  module R : sig
    val equal : t -> t -> bool
  end
end

module M : sig
  include Has_t

  (** Shadows the [t] above - [R] still refers to the first one. *)
  include sig
    type nonrec t = t
  end
end

module N : module type of M.R
