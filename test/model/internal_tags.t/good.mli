(** Canonical tag in the top-comment

    @canonical A.B *)

module type T = sig type t end

include T
(** Status tags on an [include]. Only the first one will be taken into account.

    @open
    @closed
    @inline *)

type u
(** Canonical on a type.

    @canonical A.u *)

module M : sig end
(** Canonical on a module.

    @canonical A.B *)
