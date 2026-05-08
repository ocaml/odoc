Repro of #1431. The unresolvable substitution is logged as a non-fatal
internal warning rather than triggering an assertion failure.

  $ ocamlc -bin-annot -c legacy.mli
  $ ocamlc -bin-annot -c top.mli
  $ odoc compile legacy.cmti
  $ odoc compile -I . top.cmti
  File "top.cmti":
  Warning: apply_inner_substs: fragmap failed: Unresolved module path resolved(Components/2) (Local id found: Components/2)
