(** [A] refers to {!B.t}, and to {!Missing}, which does not exist. *)

type t

(** {!modules: B} *)

module M : sig
  (** Inside [M], {!t} is [A]'s [t]. *)

  val v : t
  (** @raise Not_found when {!B.t} is empty. *)
end
