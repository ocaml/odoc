(** Status tags in the top-comment.

    @inline *)

(** Canonical on an [include].

    @canonical X.Y *)
include sig end

(** Canonical on a value.

    @canonical X.x *)
val x : int

(** Canonical in a floating comment.

    @canonical X.Y *)

(** Status tags on a value.

    @inline *)
val x : int
