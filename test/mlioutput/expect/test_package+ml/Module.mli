(** {0 Module Module}
*)

(** Foo.
*)

(**
  The module needs at least one signature item, otherwise a bug causes the compiler to drop the module comment (above). See 
  {{: https://caml.inria.fr/mantis/view.php?id=7701}https://caml.inria.fr/mantis/view.php?id=7701}.
*)
val foo : unit

module type S = sig
  
  type t
  
  type u
  
  type 'a v
  
  type ('a, 'b) w
  
  module M : sig
    end
  end

module type S1

module type S2 = sig
  
  type t
  
  type u
  
  type 'a v
  
  type ('a, 'b) w
  
  module M : sig
    end
  end

module type S3 = sig
  
  type t = int
  
  type u = string
  
  type 'a v
  
  type ('a, 'b) w
  
  module M : sig
    end
  end

module type S4 = sig
  
  type u
  
  type 'a v
  
  type ('a, 'b) w
  
  module M : sig
    end
  end

module type S5 = sig
  
  type t
  
  type u
  
  type ('a, 'b) w
  
  module M : sig
    end
  end

type ('a, 'b) result

module type S6 = sig
  
  type t
  
  type u
  
  type 'a v
  
  module M : sig
    end
  end

module M' : sig ... end

module type S7 = sig
  
  type t
  
  type u
  
  type 'a v
  
  type ('a, 'b) w
  
  module M = M'
  end

module type S8 = sig
  
  type t
  
  type u
  
  type 'a v
  
  type ('a, 'b) w
  end

module type S9 = sig
  end

module Mutually : sig ... end

module Recursive : sig ... end
