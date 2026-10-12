(** Features introduced in OCaml 5.6. *)

type ext = external "gmp"
(** An external type declaration. *)

external classic : int -> int = "caml_classic" "caml_classic_native"
(** A classic external declaration. *)

external alias_with_type : int -> int = classic
(** A primitive alias declaring its type. *)

external alias_no_type = classic
(** A primitive alias leaving its type implicit. *)
