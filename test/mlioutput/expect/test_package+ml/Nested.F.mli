(** {0 Module Nested.F}
*)

(** This is a functor F.
*)

(** Some additional comments.
*)

(** {1:type Type}
*)

(** {1:parameters Parameters}
*)

module Arg1 : sig
  
  (** {3:type Type}
  *)
  
  (**
    Some type.
  *)
  type t
  
  (** {3:values Values}
  *)
  
  (**
    The value of y.
  *)
  val y : t
  end

module Arg2 : sig
  
  (** {3:type Type}
  *)
  
  (**
    Some type.
  *)
  type t
  end

(** {1:signature Signature}
*)

(**
  Some type.
*)
type t = Arg1.t * Arg2.t
