Similar to Astring library.

  $ ocamlc -c -o a_x.cmo a_x.ml -bin-annot -I .
  $ ocamlc -c a.mli -bin-annot -I .
  $ ocamlc -c a.ml -bin-annot -I .

  $ odoc compile --impl a.ml -I . a.cmti

  $ odoc link -I . a.odoc

  $ odoc html-generate --indent -o html a.odocl

Look if all the source files are generated:

  $ find html | sort
  html
  html/A
  html/A/A.ml.html
  html/A/X
  html/A/X/index.html
  html/A/index.html

