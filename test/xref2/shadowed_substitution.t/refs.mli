type t := int
type t = string

val y : t
(** See {!t}. *)

module M := Stdlib.List
module M : sig type a end

val m : unit
(** See {!M} and {!M.a}. *)

module type S := sig end
module type S = sig type b end

val s : unit
(** See {!S} and {!S.b}. *)
