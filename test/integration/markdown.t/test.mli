(** {1 Section 1} *)

type t = int
(** A very important type *)

(** {2 Section 2} *)

val v : t
(** A very important value *)

module List : sig
  type 'a t = 'a list
  val head : 'a t -> 'a option
  val headExn : 'a t -> 'a
end

module type X = sig
  type t = int
end

module type T = sig
  include X
end
