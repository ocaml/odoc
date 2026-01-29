Test for GitHub issue #930: Crash when substituting for the same name at different arities.

This tests that inline TypeSubstitution items (type 'a t := unit) are correctly
applied during include expansion, preventing crashes and producing correct output.

  $ ocamlc -c -bin-annot test.mli
  $ ocamlc -c -bin-annot edge_cases.mli

Compile and link both test files:

  $ odoc compile test.cmti
  $ odoc compile edge_cases.cmti
  $ odoc link test.odoc
  $ odoc link edge_cases.odoc

=== Test 1: Original MWE from issue #930 ===

The key test is that odoc doesn't crash with "Invalid_argument(List.fold_left2)".
Check that includes work correctly - the TypeSubstitution is applied when
S1 is included in S2 and S2 is included in S3:

  $ odoc_print test.odocl -r S2.x | jq -c '.type_.Constr[0]'
  {"`Resolved":{"`CoreType":"unit"}}

  $ odoc_print test.odocl -r S3.x | jq -c '.type_.Constr[0]'
  {"`Resolved":{"`CoreType":"unit"}}

=== Test 2: Issue #1385 - Creators_base with nested types ===

Check that S0_with_creators_base compiles without crashing and has the
concat function with simplified types (t -> t):

  $ odoc_print test.odocl -r S0_with_creators_base.concat | jq -c '.type_.Arrow[1].Constr[0]'
  {"`Resolved":{"`Identifier":{"`Type":[{"`ModuleType":[{"`Root":["None","Test"]},"S0_with_creators_base"]},"t"]}}}

  $ odoc_print test.odocl -r S0_with_creators_base.concat | jq -c '.type_.Arrow[2].Constr[0]'
  {"`Resolved":{"`Identifier":{"`Type":[{"`ModuleType":[{"`Root":["None","Test"]},"S0_with_creators_base"]},"t"]}}}

=== Test 3: Deeply nested includes ===

Level5 goes through 5 levels of includes with TypeSubstitution at Level1.
The substitution should be applied correctly through all levels:

  $ odoc_print edge_cases.odocl -r Level5.x | jq -c '.type_.Constr[0]'
  {"`Resolved":{"`CoreType":"unit"}}

=== Test 4: Multiple paths to same signature ===

Multipath_use1 and Multipath_use2 both include variations of Multipath_base:

  $ odoc_print edge_cases.odocl -r Multipath_use1.x | jq -c '.type_.Constr[0]'
  {"`Resolved":{"`CoreType":"unit"}}

  $ odoc_print edge_cases.odocl -r Multipath_use2.x | jq -c '.type_.Constr[0]'
  {"`Resolved":{"`CoreType":"unit"}}

=== Test 5: Verify HTML generation succeeds and shows correct types ===

  $ odoc html-generate test.odocl -o html --indent
  $ odoc html-generate edge_cases.odocl -o html --indent

S2.x should show "unit" (TypeSubstitution applied through include of S1):

  $ grep "val.*x" html/Test/module-type-S2/index.html | sed 's/<[^>]*>//g' | grep -o "val x.*" | head -1
  val x : unit

S3.x should show "unit" (TypeSubstitution applied through include of S2):

  $ grep "val.*x" html/Test/module-type-S3/index.html | sed 's/<[^>]*>//g' | grep -o "val x.*" | head -1
  val x : unit

Level5.x should show "unit" after 5 levels of nested includes:

  $ grep "val.*x" html/Edge_cases/module-type-Level5/index.html | sed 's/<[^>]*>//g' | grep -o "val x.*" | head -1
  val x : unit

=== Test 6: Verify TypeSubstitutions in includes are correctly applied ===

In S2 and S3, the TypeSubstitution from S1 should be applied (not shown as "type subst"):

  $ grep -c "type subst" html/Test/module-type-S2/index.html 2>/dev/null || true
  0

  $ grep -c "type subst" html/Test/module-type-S3/index.html 2>/dev/null || true
  0

Level5 should have no TypeSubstitution visible (applied through nested includes):

  $ grep -c "type subst" html/Edge_cases/module-type-Level5/index.html 2>/dev/null || true
  0

Note: S1 still shows its TypeSubstitution declaration because that's where it's defined.
This is expected - the substitution is only applied when the signature is included:

  $ grep -c "type subst" html/Test/module-type-S1/index.html
  1
