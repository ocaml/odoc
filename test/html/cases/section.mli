(* Blank lines are needed because of
   https://caml.inria.fr/mantis/view.php?id=7701. *)

(** This is the module comment. Eventually, sections won't be allowed in it. *)

(** {2 Empty section} *)

(** {2 Text only}

    Foo bar. *)

(** {2 Aside only} *)

(** Foo bar. *)

(** {2 Value only} *)

val foo : unit

(** {2 Empty section}

    {2 within a comment}

    {3 and one with a nested section} *)

(** {2 {e This} [section] {b title} {_has} {^markup}}

    But links are impossible thanks to the parser, so we never have trouble
    rendering a section title in a table of contents – no link will be nested
    inside another link. *)
