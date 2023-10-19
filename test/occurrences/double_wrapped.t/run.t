This test simulates the conditions when a dune user write a toplevel module.

The module C is not exposed in the handwritten toplevel module.
The module A and B are exposed.
The module B depends on both B and C, the module C only depends on A.

  $ ocamlc -c -o main__.cmo main__.ml -bin-annot -w -49 -no-alias-deps -I .
  $ ocamlc -c -open Main__ -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c -open Main__ -o main__C.cmo c.ml -bin-annot -I .
  $ ocamlc -c -open Main__ -o main__B.cmo b.ml -bin-annot -I .
  $ ocamlc -c -open Main__ main.ml -bin-annot -I .

Passing the count-occurrences flag to odoc compile makes it collect the
occurrences information.


  $ odoc compile --count-occurrences -I . main__A.cmt
  $ odoc compile --count-occurrences -I . main__C.cmt
  $ odoc compile --count-occurrences -I . main__B.cmt
  $ odoc compile --count-occurrences -I . main__.cmt
  $ odoc compile --count-occurrences -I . main.cmt

  $ odoc link -I . main.odoc
  $ odoc link -I . main__A.odoc
  $ odoc link -I . main__B.odoc
  File "main__B.odoc":
  Warning: Failed to lookup value identifier((root Main__B).Z, false).y Parent_module: Lookup failure (module): (root Main__B).Z
  File "main__B.odoc":
  Warning: Failed to lookup value identifier((root Main__B).Y, false).x Parent_module: Lookup failure (module): (root Main__B).Y
  $ odoc link -I . main__C.odoc
  File "main__C.odoc":
  Warning: Failed to lookup value identifier((root Main__C).Y, false).x Parent_module: Lookup failure (module): (root Main__C).Y
  $ odoc link -I . main__.odoc

The count occurrences command outputs a marshalled hashtable, whose keys are
odoc identifiers, and whose values are integers corresponding to the number of
uses.

  $ odoc count-occurrences -I . -o occurrences.txt

  $ du -h occurrences.txt
  4.0K	occurrences.txt

The occurrences_print executable, available only for testing, unmarshal the file
and prints the number of occurrences in a readable format.

Uses of A and B are counted correctly, with the path rewritten correctly.
Uses of C are not counted, since the canonical destination (generated by dune) does not exist.
Uses of values Y.x and Z.y (in b.ml) are not counted since they come from a "local" module.
Uses of values Main__.C.y and Main__.A.x are not rewritten since we use references instead of paths.

  $ occurrences_print occurrences.txt | sort
  Main was used directly 0 times and indirectly 11 times
  Main.A was used directly 4 times and indirectly 6 times
  Main.A.(||>) was used directly 1 times and indirectly 0 times
  Main.A.M was used directly 2 times and indirectly 0 times
  Main.A.t was used directly 1 times and indirectly 0 times
  Main.A.x was used directly 2 times and indirectly 0 times
  Main.B was used directly 1 times and indirectly 0 times
  string was used directly 1 times and indirectly 0 times
