Test markdown-generate with multiple .odocl files in a single invocation:

  $ ocamlc -c -bin-annot a.mli
  $ ocamlc -c -bin-annot b.mli
  $ ocamlc -c -bin-annot c.mli
  $ odoc compile --package test a.cmti
  $ odoc compile --package test b.cmti
  $ odoc compile --package test c.cmti
  $ odoc link a.odoc
  $ odoc link b.odoc
  $ odoc link c.odoc

Generate with separate invocations:
  $ odoc markdown-generate a.odocl -o markdown-separate
  $ odoc markdown-generate b.odocl -o markdown-separate
  $ odoc markdown-generate c.odocl -o markdown-separate

Generate with multiple files in one invocation:
  $ odoc markdown-generate a.odocl b.odocl c.odocl -o markdown-multi

Verify both approaches produce identical output:
  $ diff -r markdown-separate markdown-multi
