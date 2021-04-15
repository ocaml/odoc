(** We expect this module to be referenced through Test.X.

    @canonical Test.X *)

type t

(** @canonical Test.X_m *)
module M : sig
  type t
end

module N : sig
  (** @canonical Test.X_n *)

  type t
end
