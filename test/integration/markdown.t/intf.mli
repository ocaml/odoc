(** Synopsis.

    Rest of preamble. *)

(** Floating comment at the top. *)

type t
(** Doc for [type t]. *)

val x : t
(** Doc for [val x]. *)

type a = t
(** Type alias *)

(** Doc for [type b]. *)
type b = A  (** Doc for [A]. *) | B  (** Doc for [B]. *)

type c = { a : int;  (** Doc for [a]. *) b : int  (** Doc for [b]. *) }
(** Doc for [type c]. *)

val y : [ `One  (** Doc for [`One]. *) | `Two  (** Doc for [`Two]. *) ]
(** Polymorphic variant. *)

(** Floating comment. *)

val z : t -> (t -> t) -> foo:t -> ?bar:t -> [ `One of t ] -> t * t
(** Type complicated enough to be rendered differently. *)

(** Outer doc for [M]. *)
module M : sig
  (** Inner doc for [M]. *)

  type t
end

module N : sig
  (** Doc for [N]. *)

  type t
end

module type S = sig
  (** Doc for [S]. *)

  type t
end
