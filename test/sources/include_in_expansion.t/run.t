When rewriting Main__A's source parents, some source parents have to be updated,
and some have to be kept.

  $ ocamlc -c -o b.cmo b.ml -bin-annot -I .
  $ ocamlc -c -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c main.ml -bin-annot -I .

  $ odoc compile --impl b.ml -I . b.cmt
  $ odoc compile --impl a.ml -I . main__A.cmt
  $ odoc compile --impl main.ml -I . main.cmt

  $ odoc link -I . main.odoc

  $ odoc html-generate --indent -o html main.odocl

In Main.A, the source parent of value x should be rewritten to Main.A, while the
source parent of value y should be left to B. (Not working yet)

  $ grep source_link html/Main/A/index.html
         <a href="A.ml.html#def-B0" class="source_link">Source</a>
       <a href="A.ml.html#def-Main__A0" class="source_link">Source</a>
