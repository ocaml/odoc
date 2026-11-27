(**/**)
module type S = sig val x : int end
module type S := S
(**/**)

module X : S
