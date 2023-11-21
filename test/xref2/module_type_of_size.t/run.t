Generate some code that's pretty bad for the odoc 2.0-2.3 implentation of
`module type of`. The problem is that the size of the files becomes
enormous very quickly.

Start with a base module:

  $ cat m1.mli
  module type M = sig
    type t
  end
  
  module N : sig type t end
  module T : sig type t type u end

Script to compile the modules:
  $ ./compile.sh
  + ocamlc -c m1.mli -bin-annot
  + ocamlc -c m2.mli -bin-annot
  + ocamlc -c m3.mli -bin-annot
  + ocamlc -c m4.mli -bin-annot
  + ocamlc -c m5.mli -bin-annot
  + ocamlc -c m6.mli -bin-annot
  + ocamlc -c m7.mli -bin-annot
  + ocamlc -c m8.mli -bin-annot
  + ocamlc -c m9.mli -bin-annot
  + ocamlc -c m10.mli -bin-annot
  + ocamlc -c m11.mli -bin-annot
  + ocamlc -c m12.mli -bin-annot
  + ocamlc -c m13.mli -bin-annot
  + ocamlc -c m14.mli -bin-annot

And one to run odoc on the files:
  $ ./odoc.sh
  + odoc compile -I . m1.cmti
  + odoc compile -I . m2.cmti
  + odoc compile -I . m3.cmti
  + odoc compile -I . m4.cmti
  + odoc compile -I . m5.cmti
  + odoc compile -I . m6.cmti
  + odoc compile -I . m7.cmti
  + odoc compile -I . m8.cmti
  + odoc compile -I . m9.cmti
  + odoc compile -I . m10.cmti
  + odoc compile -I . m11.cmti
  + odoc compile -I . m12.cmti
  + odoc compile -I . m13.cmti
  + odoc compile -I . m14.cmti


None of the files really ought to be bigger than 1M!

  $ find . -size +1000000c | sort 
  ./m12.odoc
  ./m13.odoc
  ./m14.odoc


