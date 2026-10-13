module Base : sig type b end

module type S = sig
  type t := int

  val x : t
  (** See {!t}. *)

  module M := Base

  val m : unit
  (** See {!M} and {!M.b}. *)
end

module I : sig
  type t = string

  include S
end
