open Odoc_model.Lang
(** The purpose of this module is to expand module type
    expressions of the form [module type of]. This is necessary
    so odoc can keep track of these expressions but mainting the
    semantics of the underlying OCaml. *)

val signature : Env.t -> Signature.t -> Signature.t
(** Expand all of the [module type of] expressions within the
    given signature *)
