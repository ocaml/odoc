A quick test to repro the issue found in #944

  $ ocamlc -bin-annot -c foo.mli

  $ odoc compile foo.cmti
  $ odoc link foo.odoc

  $ odoc html-generate --indent -o html/ foo.odocl
