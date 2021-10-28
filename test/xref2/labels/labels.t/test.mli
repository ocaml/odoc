(** {1:A First label} *)

(** {1:B Floating label} *)

module M : sig
  (** {1:C First label of M} *)

  (** {1:D Floating label in M} *)

  (** {1:B Potentially conflicting label} *)
end

module N : sig
  (** *)

  (** {1:B An other conflicting label} *)

  (** {!B} {!M.B} *)
end

(** {1:B Dupplicate B}

    Define [B] again in the same scope. *)

(** References to the labels:

    {!A} {!B} {!M.C} {!M.D} {!M.B} {!N.B} *)
