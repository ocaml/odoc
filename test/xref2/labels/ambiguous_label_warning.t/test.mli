(** *)

(** {1:foo H1}

    {1:foo H2} *)

(** {1 heading} *)

(** {1 heading} *)

(** {!heading} *)

module M : sig
  (** Test of scoping rules *)

  (** {1 heading} *)

  (** {!heading} should be unambiguous *)
end

