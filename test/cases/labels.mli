(** {1:L1 Attached to unit} *)

(** {1:L2 Attached to nothing} *)

(** {1:L3 Attached to module} *)
module A : sig end

(** {1:L4 Attached to type} *)
type t

(** {1:L5 Attached to value} *)
val f : t

(** {1:L5bis Attached to external} *)
external e : unit -> t = "t"

(** {1:L6 Attached to module type} *)
module type S = sig end

(** {1:L6 Attached to class} *)
class c : object end

(** {1:L7 Attached to class type} *)
class type cs = object end

(** {1:L8 Attached to exception} *)
exception E

type x = ..

(** {1:L9 Attached to extension} *)
type x += X

(** {1:L10 Attached to module subst} *)
module S := A

(** {1:L11 Attached to type subst} *)
type s := t

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
  *)
