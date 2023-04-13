A quick test to repro the issue found in #944

  $ ocamlc -bin-annot -c foo.mli

  $ odoc compile foo.cmti
  $ odoc link foo.odoc --enable-missing-root-warning
  File "foo.odoc":
  Warning: Couldn't find the following modules:
    Stdlib
