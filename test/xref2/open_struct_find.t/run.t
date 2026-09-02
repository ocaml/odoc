Items in a unit's interface can refer to modules bound by a top-level
extended open (here `open struct ... end`). Resolving such a path from another unit goes through
Find rather than Env; Find recurses into Open expansion items the same way
it does for includes. Previously the lookup failed with a Find failure:

  $ ocamlc -c -bin-annot a.ml
  $ ocamlc -c -bin-annot -I . b.mli
  $ odoc compile a.cmt
  $ odoc compile -I . b.cmti
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
