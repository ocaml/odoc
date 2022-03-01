(** This test cases exercises stop comments. *)

val foo : int
(** This is normal commented text. *)

(** The next value is [bar], and it should be missing from the documentation.
    There is also an entire module, [M], which should also be hidden. It
    contains a nested stop comment, but that stop comment should not turn
    documentation back on in this outer module, because stop comments respect
    scope. *)

(**/**)

val bar : int
(** OMG! *)

module M :
sig
  val baz : int

  (**/**)
end

(**/**)

(** Documentation is on again.

    Now, we have a nested module, and it has a stop comment between its two
    items. We want to see that the first item is displayed, but the second is
    missing, and the stop comment disables documenation only in that module, and
    not in this outer module. *)

module N :
sig
  val quux : int

  (**/**)

  val omg : int
end

val lol : int

(** The first comment can also be a stop-comment. The test case
    [stop_first_comment.mli] is testing the same thing but at the toplevel. We
    should see [bar] inside {!O}. *)

module O :
sig
  (**/**)

  val foo : int

  (**/**)

  val bar : int
end

(** The top-comment computation must not mess with stop comments. *)

module P :
sig
  (** Doc. *)

  (**/**)

  val foo : int

  (**/**)

  val bar : int
end
