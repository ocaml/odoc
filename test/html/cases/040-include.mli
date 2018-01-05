module type Not_inlined =
sig
  type t
end

include Not_inlined

module type Inlined =
sig
  type u
end

include Inlined
(** @inline *)
