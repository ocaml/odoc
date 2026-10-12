(** Module A defines a signature whose inline include declares the type. *)

module type S = sig
  include sig
    type t

    val x : t
    val y : t -> t
  end
end
