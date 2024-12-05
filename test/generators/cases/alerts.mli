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

  (* On OCaml 4.04 to 4.06, this top-comment is lost by the parser if there's
     no empty line before it. *)
  (** Top-comment. *)
end
[@@deprecated "A"]

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

