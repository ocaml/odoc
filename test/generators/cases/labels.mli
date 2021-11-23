(** {1:L1 Attached to unit} *)

(** {1:L2 Attached to nothing} *)

module A : sig end
(** {1:L3 Attached to module} *)

type t
(** {1:L4 Attached to type} *)

val f : t
(** {1:L5 Attached to value} *)

external e : unit -> t = "t"
(** {1:L5bis Attached to external} *)

module type S = sig end
(** {1:L6 Attached to module type} *)

class c : object end
(** {1:L7 Attached to class} *)

class type cs = object end
(** {1:L8 Attached to class type} *)

exception E
(** {1:L9 Attached to exception} *)

type x = ..

(** {1:L10 Attached to extension} *)
type x += X

module S := A
(** {1:L11 Attached to module subst} *)

type s := t
(** {1:L12 Attached to type subst} *)

type u = A'  (** {1:L13 Attached to constructor} *)

type v = { f : t  (** {1:L14 Attached to field} *) }

(** Testing that labels can be referenced
    - {!L1}
    - {!L2}
    - {!L3}
    - {!L4}
    - {!L5}
    - {!L6}
    - {!L7}
    - {!L8}
    - {!L9}
    - {!L10}
    - {!L11}
    - {!L12}
    - {!L13}
    - {!L14}
  *)
