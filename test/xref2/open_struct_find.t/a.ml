(** Top comment. *)

open struct
  module M = struct
    type t = int
  end
end

module type S = sig
  val v : M.t
end
