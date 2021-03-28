(** {0 Module Functor}
*)

module type S = sig
  
  type t
  end

module type S1 = sig
  
  (** {2:parameters Parameters}
  *)
  
  module _ : sig
    
    type t
    end
  
  (** {2:signature Signature}
  *)
  
  type t
  end

module F1 (Arg : S) : S

module F2 (Arg : S) : S with type t = Arg.t

module F3 (Arg : S) : sig ... end

module F4 (Arg : S) : S

module F5 () : S
