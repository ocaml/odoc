(** We expect this module to be referenced through Test.X.

    @canonical Test.X *)

type t
(** @canonical Test.t *)

(** @canonical Test.X_out *)
module Out : sig
  type t
end

module In : sig
  (** @canonical Test.X_in *)

  (** @canonical Test.v *)
  type t
end
