(* Why is there a normal comment here: The mli file should not be empty for
   ocaml < 4.07 to pick the standalone comment. See
   https://github.com/ocaml/ocaml/issues/7701 and
   https://github.com/ocaml/ocaml/pull/1693 *)

(** {2:splice_me Splice me}
    Should output only the heading's text:
    {!splice_me}
    {!Foo.splice_me}
    {!page.splice_me} *)
