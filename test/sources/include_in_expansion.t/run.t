Checking that source parents are kept, using include.

  $ ocamlc -c -o b.cmo b.ml -bin-annot -I .
  $ ocamlc -c -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

  $ odoc compile --impl b.ml -I . b.cmt
  $ odoc compile --impl a.ml -I . main__A.cmt
  $ odoc compile --impl main.ml -I . main.cmt

  $ odoc link -I . main.odoc
  $ odoc link -I . main__A.odoc

  $ odoc html-generate --indent -o html main.odocl
  $ odoc html-generate --hidden --indent -o html main__A.odocl

In Main.A, the source parent of value x should be to Main__A, while the
source parent of value y should be left to B.

  $ grep source_link html/Main/A/index.html
         <a href="../../B/B.ml.html#def-B0" class="source_link">Source</a>
       <a href="../../Main__A/Main__A.ml.html#def-Main__A0" class="source_link">
