(** {0 Module Include_sections}
*)

(**
  A module type.
*)
module type Something = sig
  
  val something : unit
  
  (** {2:something-1 Something 1}
  *)
  
  (** foo
  *)
  
  val foo : unit
  
  (** {3:something-2 Something 2}
  *)
  
  (**
    foo bar
  *)
  val bar : unit
  
  (** {2:something-1-bis Something 1-bis}
  *)
  
  (** Some text.
  *)
  end

(** Let's include {!Something} once
*)

val something : unit

(** {1:something-1 Something 1}
*)

(** foo
*)

val foo : unit

(** {2:something-2 Something 2}
*)

(**
  foo bar
*)
val bar : unit

(** {1:something-1-bis Something 1-bis}
*)

(** Some text.
*)

(** {1:second-include Second include}
*)

(**
  Let's include {!Something} a second time: the heading level should be shift here.
*)

val something : unit

(** {2:something-1 Something 1}
*)

(** foo
*)

val foo : unit

(** {3:something-2 Something 2}
*)

(**
  foo bar
*)
val bar : unit

(** {2:something-1-bis Something 1-bis}
*)

(** Some text.
*)

(** {2:third-include Third include}
*)

(** Shifted some more.
*)

val something : unit

(** {3:something-1 Something 1}
*)

(** foo
*)

val foo : unit

(** {4:something-2 Something 2}
*)

(**
  foo bar
*)
val bar : unit

(** {3:something-1-bis Something 1-bis}
*)

(** Some text.
*)

(**
  And let's include it again, but without inlining it this time: the ToC shouldn't grow.
*)

val something : unit

(** {3:something-1 Something 1}
*)

(** foo
*)

val foo : unit

(** {4:something-2 Something 2}
*)

(**
  foo bar
*)
val bar : unit

(** {3:something-1-bis Something 1-bis}
*)

(** Some text.
*)
