Items in a unit's interface can refer to modules bound by a top-level
extended open (here `open struct ... end`). Resolving such a path from another unit goes through
Find rather than Env, and Find does not look inside Open expansion items:

  $ ocamlc -c -bin-annot a.ml
  $ ocamlc -c -bin-annot -I . b.mli
  $ odoc compile a.cmt
  $ odoc compile -I . b.cmti
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  File "b.odoc":
  Warning: Failed to lookup type resolved(root(A)).M.t Parent_module: Find failure
