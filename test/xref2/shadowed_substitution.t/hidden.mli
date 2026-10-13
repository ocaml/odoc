(**/**)
type t = int
type t := t
type t := t list
module type S = sig val x : int end
module type S := S
module Base : sig module Nested : sig type n = bool end end
module M := Base
module M := M.Nested
(**/**)

val u : t
module X : S
val w : M.n
