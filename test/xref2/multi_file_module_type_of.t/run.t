Module type of across multiple modules
======================================

The logic to keep looping when we have nested `module type of` expressions needs
to know when to stop. If the unexpanded `module type of` expression is actually in
a separate module, no matter how many times we try it'll never work. Thus there is
some logic to check before it loops to see if the signature has changed at all.
If not, it doesn't loop. Without this logic, the following test would loop forever

Here are the test files:

  $ cat test0.mli
  type t

  $ cat test1.mli
  module S : module type of Test0
  
  $ cat test2.mli
  module T : module type of Test1.S
  
  $ ocamlc -c -bin-annot test0.mli
  $ ocamlc -c -bin-annot test1.mli
  $ ocamlc -c -bin-annot test2.mli

In this instance, module S will not be expanded because we are not providing an
odoc file for `Test0` - so there will be a warning when we run `odoc compile`
on test1.cmti:

  $ odoc compile --package foo test1.cmti --enable-missing-root-warning
  File "test1.cmti":
  Warning: Couldn't find the following modules:
    Test0

Similarly, module `T` also can not be expanded, therefore we expect
another warning when we run `odoc compile` on test2.cmti:

  $ odoc compile --package foo test2.cmti -I . --enable-missing-root-warning
  File "test2.cmti":
  Warning: Couldn't find the following modules:
    Test0

Crucially though, we do expect this command to have terminated!

