(** {0 Module Nested}
*)

(** This comment needs to be here before #235 is fixed.
*)

(** {1:module Module}
*)

(**
  This is module X.
*)
module X : sig ... end

(** {1:module-type Module type}
*)

(**
  This is module type Y.
*)
module type Y = sig
  
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

(** {1:functor Functor}
*)

(**
  This is a functor F.
*)
module F (Arg1 : Y) (Arg2 : sig ... end) : sig ... end

(** {1:class Class}
*)

(**
  This is class z.
*)
class virtual  z : object ... end

class virtual  inherits : object ... end
