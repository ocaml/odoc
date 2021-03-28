(** {0 Module Functor.F2}
*)

(** {1:parameters Parameters}
*)

module Arg : sig
  
  type t
  end

(** {1:signature Signature}
*)

type t = Arg.t
