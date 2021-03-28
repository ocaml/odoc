(** {0 Module Recent}
*)

module type S = sig
  end

module type S1 = sig
  
  (** {2:parameters Parameters}
  *)
  
  module _ : sig
    end
  
  (** {2:signature Signature}
  *)
  end

type variant = 
  | A
  | B of int
  | C
  (**
    foo
  *)
  | D
  (**
    {e bar}
  *)
  | E of {
  a : int;}

type _ gadt = 
  | A : int gadt
  | B : int -> string gadt
  (**
    foo
  *)
  | C : {
  a : int;} -> unit gadt

type polymorphic_variant = [ 
  | `A
  | `B of int
  | `C
  (**
    foo
  *)
  | `D
  (**
    bar
  *) ]

type empty_variant = |

type nonrec nonrec_ = int

type empty_conj = 
  | X : [< `X of & 'a & int * float ] -> empty_conj

type conj = 
  | X : [< `X of int & [< `B of int & float ] ] -> conj

val empty_conj : [< `X of & 'a & int * float ]

val conj : [< `X of int & [< `B of int & float ] ]

module Z : sig ... end

module X : sig ... end

module type PolyS = sig
  
  type t = [ 
    | `A
    | `B ]
  end
