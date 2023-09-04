Source code rendering needs the same compilation order as cmts.

As a consequence, the dependencies should be taken from the cmt, when source
code rendering is enabled.

  $ ocamlc -c b.ml -bin-annot
  $ ocamlc -c a.mli -I . -bin-annot
  $ ocamlc -c a.ml -I . -bin-annot

[a.cmti] does not depend on B, while its implementation [a.cmt] depends on B.

  $ odoc compile-deps a.cmti | cut -d ' ' -f 1 | sort
  A
  CamlinternalFormatBasics
  Stdlib

  $ odoc compile-deps a.cmt | cut -d ' ' -f 1 | sort
  A
  B
  CamlinternalFormatBasics
  Stdlib

Must be the merge of both dependencies:

  $ odoc compile-deps a.cmti a.cmt | cut -d ' ' -f 1 | sort
  A
  B
  CamlinternalFormatBasics
  Stdlib
