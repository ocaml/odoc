Let's check for expected errors.

  $ cat top1.mld
  {0 Top1}
  This is the top1 page.

We need to match parents with children
  $ odoc compile -c dummy top1.mld
  $ odoc compile -I . --parent top1 sub1.mld

This is a different code-path:
  $ odoc compile top1.mld --child foo
  $ odoc compile -I . --parent top1 sub1.mld

And these need to specify compilation unit children as well as mld children
  $ ocamlc -c -bin-annot m1.mli
  $ odoc compile m1.cmti -I . --parent top1
  ERROR: File "m1.cmti":
  Specified parent is not a parent of this file
  [1]

Parents must be pages
  $ odoc compile top1.mld --child M1
  $ odoc compile m1.cmti -I . --parent top1
  $ odoc compile sub1.mld -I . --parent module-M1
  ERROR: Expecting page as parent
  [1]

Linking checks the children are all present:
  $ odoc compile top1.mld --child foo
  $ odoc link page-top1.odoc -I .
  File "page-top1.odoc":
  Warning: Failed to lookup child page foo

There are old and new CLI options. We are checking than an error is produced
when we mix the two kinds.
  $ odoc compile --parent bla --parent-id blabla m1.mli
  Either --parent or --parent-id should be specified, not both.
  [2]
  $ odoc compile --parent bla --package blabla m1.mli
  Either --package or --parent should be specified, not both.
  [2]
  $ odoc compile --package bla --child foo -- m1.mli
  --child cannot be passed with --package.
  [2]
  $ odoc compile --parent-id bla --child foo -- m1.mli
  --child cannot be passed with --parent-id.
  [2]
  $ odoc compile --package bla --output-dir foo -- m1.mli
  --output-dir can only be passed with --parent-id.
  [2]
  $ odoc compile --parent bla --output-dir foo -- m1.mli
  --output-dir can only be passed with --parent-id.
  [2]

