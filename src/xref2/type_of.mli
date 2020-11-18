(** The purpose of this module is to expand module type
    expressions of the form [module type of]. This is necessary
    so odoc can keep track of these expressions but mainting the
    semantics of the underlying OCaml. *)
open Odoc_model.Lang

(** Expand all of the [module type of] expressions within the
    given signature *)
val signature : Env.t -> Signature.t -> Signature.t
