(** {0 Module Section}
*)

(** This is the module comment. Eventually, sections won't be allowed in it.
*)

(** {1:empty-section Empty section}
*)

(** {1:text-only Text only}
*)

(** Foo bar.
*)

(** {1:aside-only Aside only}
*)

(** Foo bar.
*)

(** {1:value-only Value only}
*)

val foo : unit

(** {1:empty-section Empty section}
*)

(** {1:within-a-comment within a comment}
*)

(** {2:and-one-with-a-nested-section and one with a nested section}
*)

(**
  {1:this-section-title-has-markup {e This} section {b title} {_ has} 
  {^ markup}}
*)

(**
  But links are impossible thanks to the parser, so we never have trouble rendering a section title in a table of contents – no link will be nested inside another link.
*)
