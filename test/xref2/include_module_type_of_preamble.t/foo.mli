(** Preamble for Foo. *)

(** Preamble for O. *)
module O : sig
  val x : int
end

include module type of O (** @inline *)
