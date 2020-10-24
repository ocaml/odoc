Multiple `module type of` statements
====================================

The way the `module type of` logic works is that an initial pass is made over the
signature looking for `module type of` expressions and expanding them there and then.
If there are 'nested' `module type of` expressions, that is, we're trying to find
the type of a module that either is, or contains, a `module type of` expression 
itself, then we loop.

This is a test of this nested situation. Expanding module Z requires module Y
to have been expanded.

  $ cat test.mli
  module X : sig type t end
  module Y : module type of struct include X end
  module Z : module type of Y
  

If we correctly expand all of the `module type of` expressions there should be no
warnings raised when we compile the module.

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile --package test test.cmti

