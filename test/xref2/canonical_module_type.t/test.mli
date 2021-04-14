(** @canonical Test.Y *)
module type A = sig
  type t
end

module type B = sig
  (** The canonical tag is in the top-comment.
      @canonical Test.X *)

  type t
end

module type X = B

module type Y = A

module type Z = A
