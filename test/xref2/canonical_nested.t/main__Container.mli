module Test : sig
  (** Nested canonical test *)

  module A : sig
    (** @canonical Main.Container.Test.B *)
    
    type t
  end

  module B = A

  type t = A.t
end


