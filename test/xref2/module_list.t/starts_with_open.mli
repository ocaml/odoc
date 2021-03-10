open External

(** Synopsis of [Starts_with_open].

    Opens should be skipped while looking up the first comment. Odoc doesn't
    render them and sometimes the typechecker remove them too.
    (eg. when they add only types) *)

(* From [External], otherwise the [open] would be dropped by the compiler *)
type t = X.t
