module Other : sig end

type t := int
type t = string

val y : t
(** See {!t}. *)

module M : sig type a end
module M := Other

val m : unit
(** See {!M} and {!M.a}. *)

include sig type i = string end
type i := int

val inc : unit
(** See {!i}. *)
