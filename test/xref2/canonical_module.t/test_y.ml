(** An other canonical unit test but this time in an .ml file.

    @canonical Test.Y *)

type t

(** @canonical Test.Y_out *)
module Out = struct
  type t
end

module In = struct
  (** @canonical Test.Y_in *)

  type t
end
