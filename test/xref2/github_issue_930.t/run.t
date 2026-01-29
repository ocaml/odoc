Test for GitHub issue #930: Crash when substituting for the same name at different arities.

This test demonstrates that odoc crashes on the MWE. The fix will update this test
to show correct behavior.

  $ ocamlc -c -bin-annot test.mli

Without the fix, odoc compile crashes with Invalid_argument("List.fold_left2"):

  $ odoc compile test.cmti 2>&1 | grep -o 'Invalid_argument("[^"]*")'
  Invalid_argument("List.fold_left2")
