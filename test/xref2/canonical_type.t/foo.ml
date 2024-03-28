module Type = Type


(* Canonical loops test *)

(** @canonical Foo.t3 *)
type t2 = int

(** @canonical Foo.t3 *)
type t3 = t2
