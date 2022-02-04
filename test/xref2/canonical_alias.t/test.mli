(** Testing some canonical edge cases *)

(** This is the simplest case *)

module A__ : sig
  (** @canonical Test.A *)
  
  type t
end

module A = A__

type test = A__.t

(** Dune-style wrapped library *)

module Wrapped__X : sig
  type t 
end

module Wrapper : sig
  
  (** @canonical Test.Wrapper.X *)
  module X = Wrapped__X

end

type test2 = Wrapper.X.t


(** Dune-style wrapped library with hand-written wrapper module *)

module Wrapped2__X : sig
  type t
end

module Wrapper2__ : sig
  
  (** @canonical Test.Wrapper2.X *)
  module X = Wrapped2__X
end

module Wrapper2 : sig
  open Wrapper2__

  module X = X
end

type test3 = Wrapper2__.X.t


(** Dune-style wrapped library with hand-written wrapper module, but wrong *)

module Wrapped3__X : sig
  type t
end

module Wrapper3__ : sig
  
  (** @canonical Test.Wrapper3.X *)
  module X = Wrapped3__X
end

module Wrapper3 : sig
  open Wrapper3__

  module X : module type of struct include X end with type t := X.t
end

val test3a : Wrapper3__.X.t

(** Non-hidden *)

module B_ : sig
  (** @canonical Test.B *)

  type t
end

module B = B_

type test4 = B_.t


(** Alias doesn't know it's canonical *)

module C_ : sig
  
  type t
end

module C = C_

(** @canonical Test.C *)
module D = C

type test5 = D.t

