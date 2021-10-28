(** *)

(** {1:foo H1} *)

module X : sig
  (** *)

  (** {1:foo H2} *)

  (** {{!foo}Expecting H2} *)
end

(** {{!foo}Expecting H1}
    {{!X.foo}Expecting H2} *)
