module O : sig
  type t := int

  val x : t

  module M := Stdlib.List
end

open O

val y : int
(** See {!t} and {!M}. *)
