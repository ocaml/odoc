module type X = sig type t end

module type S = sig
  val f : (module M : X) -> M.t
end

include S
