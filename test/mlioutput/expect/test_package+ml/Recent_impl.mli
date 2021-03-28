(** {0 Module Recent_impl}
*)

module Foo : sig ... end

module B : sig ... end

type u

module type S = sig
  
  module F : sig
    
    (** {2:parameters Parameters}
    *)
    
    module _ : sig
      end
    
    (** {2:signature Signature}
    *)
    
    type t
    end
  
  module X : sig
    end
  
  val f : F(X).t
  end

module B' = Foo.B
