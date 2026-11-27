(**/**)
type t = int
type t := t
type t := t list
module Base : sig module Nested : sig type n = bool end end
module M := Base
module M := M.Nested
(**/**)

val u : t
val w : M.n
