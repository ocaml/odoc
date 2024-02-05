(* Expansion test *)

(** testing *)

(**/**)
module H : sig
  type t
end

module S : sig
  module type X = sig
    module M : sig
      type t
    end
  end 

  module X : X with type M.t = int

  module Y : sig type t end

  module Z : module type of Y

  module A : X

  module B = H
end
(**/**)

module Test = S

