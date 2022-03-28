(* Can also inline the module types [B] and [B.Q] here. *)

module B : sig
  type t

  val a : unit
  
  module Q : sig
    module U : sig end
  end
  
  module U : sig end
  
end

include module type of B

module Q : sig
  include module type of struct
    include B.Q
  end

  module U : sig end

end

module U : sig end
