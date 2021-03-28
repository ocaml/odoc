(** {0 Module Bugs}
*)

type 'a opt = 'a option

(**
  Triggers an assertion failure when {{: https://github.com/ocaml/odoc/issues/101}https://github.com/ocaml/odoc/issues/101} is not fixed.
*)
val foo : ?bar:'a -> unit -> unit
