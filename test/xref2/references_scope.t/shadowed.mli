(** Reference to a shadowed module. References should be resolved after the
    whole signature has been added to the scope. *)

type t

module M : sig
  (** . *)

  (** Should resolve to [M.t]: {{!t} Before-shadowed}.

      Of course, it's possible to reference {!Shadowed.t}. *)

  type t

  (** Should resolve to [M.t]: {{!t} After-shadowed} *)
end

external f : int -> int = "%identity"

module N : sig
  (** [val] and [external] both define a value.

      Should resolve to [N.f]: {!f}

      Should resolve to [Shadowed.f]: {!Shadowed.f} *)

  val f : int -> int
end
