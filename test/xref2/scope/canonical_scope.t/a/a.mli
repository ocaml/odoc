module type S = sig
  module X : sig
    type t
  end
  (** @canonical Lib.X *)
end
