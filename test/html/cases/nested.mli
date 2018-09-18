
(** This is module X.

    Some additional comments. *)
module X : sig
  type t
  (** Some type. *)

  val x : t
  (** The value of x. *)
end



(** This is module type Y.

    Some additional comments. *)
module type Y = sig
  type t
  (** Some type. *)

  val y : t
  (** The value of y. *)
end


(** This is class z.

    Some additional comments. *)
class z : object

  method z : int
  (** Some method. *)
end

