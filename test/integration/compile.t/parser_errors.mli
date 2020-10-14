(** {x This is bad markup} *)
val a : int

(** {9 Bad hading level} *)
val b : int

(* {4 Heading} this should be on it's own line *)
val c : int

(** {ul {limust be followed by whitespace}} *)
val d : int

(** {limust in a ul} *)
val e : int

(** {vmust be followed by whitespace v} *)
val f : int

(** {v must be preceded by whitespacev} *)
val g : int

(** @ stray *)
val h : int

(** Expect something on the same line: *)
val i : int

(** @before *)
val j : int

(** @param *)
val k : int

(** @raise *)
val l1 : int

(** @raises *)
val l2 : int

(** @see *)
val m : int

(** @UnknownTag *)
val n : int

(** } unpaired *)
val o : int

(** ] unpaired *)
val p : int

(** This comment has bad
    } markup on the second line. *)
val r : int

(** {x bad markup} in a standalone comment. *)
