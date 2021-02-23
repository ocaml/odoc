(** {!modules: External External.X Main Internal Internal.Y} *)

(** Doc for [Internal].

    An other paragraph*)
module Internal : sig

  (** Doc for Internal.[X]. An other sentence. *)
  module Y : sig end
end
