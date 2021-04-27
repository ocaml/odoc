(** *)

(** Reference the other modules. *)
type b = B.t

(** Reference to something that doesn't exist: {!B.doesn't_exist} *)

(** Invalid syntax: {! *)
