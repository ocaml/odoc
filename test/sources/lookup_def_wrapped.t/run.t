Make sure wrapped libraries don't interfere with generating the source code.
Test both canonical paths and hidden units.
It's a simpler case than Dune's wrapping.

  $ ocamlc -c -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c -o main__B.cmo b.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

  $ odoc compile --impl a.ml -I . main__A.cmt
  $ odoc compile --impl b.ml -I . main__B.cmt
  $ odoc compile --impl main.ml -I . main.cmt

  $ odoc link -I . main__A.odoc
  $ odoc link -I . main__B.odoc
  $ odoc link -I . main.odoc

  $ odoc html-generate --indent -o html main.odocl

Look if all the source files are generated:

  $ find html | sort
  html
  html/Main
  html/Main/A
  html/Main/A/index.html
  html/Main/B
  html/Main/B/index.html
  html/Main/Main.ml.html
  html/Main/index.html
