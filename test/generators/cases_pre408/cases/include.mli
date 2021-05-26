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

module type Not_inlined_and_closed =
sig
  type v
end

include Not_inlined_and_closed
(** @closed *)

module type Not_inlined_and_opened =
sig
  type w
end

include Not_inlined_and_opened
(** @open
    @closed *)

(* This demostrates that overridden values are never rendered*)
module type Inherent_Module =
sig
  val a : t
end

include Inherent_Module


module type Dorminant_Module =
sig
  include Inherent_Module
  val a : u
end

include Dorminant_Module
