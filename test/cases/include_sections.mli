(** A module type. *)
module type Something = sig

  val something : unit

  (** {1 Something 1}

      foo *)

  val foo : unit

  (** {2 Something 2} *)

  val bar : unit
  (** foo bar *)

  (** {1 Something 1-bis}

      Some text. *)
end

(** Let's include {!Something} once *)

include Something (** @inline *)

(** {1 Second include}

    Let's include {!Something} a second time: the heading level should be shift here.  *)

include Something (** @inline *)

(** {2 Third include}

    Shifted some more.  *)

include Something (** @inline *)

(** And let's include it again, but without inlining it this time: the ToC
    shouldn't grow. *)

include Something
