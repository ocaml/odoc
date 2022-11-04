Unexpanded `module type of`
===========================

This is a test for [this issue](https://github.com/ocaml/odoc/issues/500)

  $ cat test0.mli
  type t

  $ cat test.mli
  module M: sig include module type of Test0 end
  
  $ ocamlc -c -bin-annot test0.mli
  $ ocamlc -c -bin-annot test.mli

Compiling an odoc file for `test` without compiling one for `test0` 
should _not_ result in an exception, merely a warning.

  $ odoc compile --package test test.cmti --enable-missing-root-warning
  File "test.cmti":
  Warning: Couldn't find the following modules:
    Test0
