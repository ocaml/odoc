(** An other canonical unit test but this time in an .ml file.

    @canonical Test.W *)

type t

(** @canonical Test.W_m *)
module M = struct
  type t
end

module N = struct
  (** @canonical Test.W_n *)

  type t
end
