(* Deprecated tag. *)

val a : int
[@@deprecated "a"]

(* Both attribute and tag. *)

val b : int
[@@deprecated "Shouldn't be rendered"]
(** @deprecated b. *)

(* No payload *)

val c : int
[@@deprecated]

(* At the top-level of a module. *)

module Top1 : sig
  [@@@deprecated "A"]

  (** Top-comment. *)
end

module Top2 : sig
  [@@@deprecated "A"]

  (** Top-comment. *)
end

(* Deprecated alert tag. *)

val d : int
[@@alert deprecated "A deprecated alert d"]

val d2 : int
[@@alert deprecated]

(* Custom alert tag. *)

val e : int
[@@alert e "an alert"]

(* Custom alert tag without payload. *)

val f : int
[@@alert f]

