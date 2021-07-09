module type A = sig
  type t
  val v : t
end

module type B = A

(** {!B} {!B.t} *)

