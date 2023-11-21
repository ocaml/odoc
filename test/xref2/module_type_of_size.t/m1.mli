module type M = sig
  type t
end

module N : sig type t end
module T : sig type t type u end
