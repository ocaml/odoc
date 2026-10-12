(** Module B includes [A.S] with a type substitution. *)

module M : A.S with type t := int
