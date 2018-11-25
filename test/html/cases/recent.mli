(* These tests are run on only the most recent version of the compiler that is
   explicitly supported by odoc. This allows us to test doc generation for new
   language features. *)

module type S = sig end

module type S1 = S -> S

type variant =
  | A
  | B of int
  | C (** foo *)
  | D (** {e bar} *)
  | E of {a : int}

type _ gadt =
  | A : int gadt
  | B : int -> string gadt (** foo *)
  | C : {a : int} -> unit gadt

type polymorphic_variant = [
  | `A
  | `B of int
  | `C (** foo *)
  | `D (** bar *)
]
