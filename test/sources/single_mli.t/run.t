Similar to Astring library.

  $ ocamlc -c -o a_x.cmo a_x.ml -bin-annot -I .
  $ ocamlc -c a.mli -bin-annot -I .
  $ ocamlc -c a.ml -bin-annot -I .

  $ odoc compile-impl --source-id src/a_x.ml -I . a_x.cmt
  $ odoc compile --hidden -I . a_x.cmt
  $ odoc compile-impl --source-id src/a.ml -I . a.cmt
  $ odoc compile -I . a.cmti

  $ odoc link -I . impl-a_x.odoc
  $ odoc link -I . a_x.odoc
  $ odoc link -I . impl-a.odoc
  $ odoc link -I . a.odoc

  $ odoc html-generate-source --impl impl-a_x.odocl --indent -o html a_x.ml
  $ odoc html-generate --indent -o html a_x.odocl
  $ odoc html-generate-source --impl impl-a.odocl --indent -o html a.ml
  $ odoc html-generate --indent -o html a.odocl

Look if all the source files are generated:

  $ find html | sort
  html
  html/A
  html/A/X
  html/A/X/Y
  html/A/X/Y/index.html
  html/A/X/index.html
  html/A/index.html
  html/A_x
  html/A_x/index.html
  html/src
  html/src/a.ml.html
  html/src/a_x.ml.html

Documentation for `A_x` is not generated for hidden modules:

  $ ! [ -f html/A_x/index.html ]
  [1]

Code source for `A_x` is wanted:

  $ [ -f html/root/source/a_x.ml.html ]
  [1]

`A` should contain a link to `A_x.ml.html`:

  $ grep source_link html/A/index.html
      <a href="../src/a.ml.html" class="source_link">Source</a>
       <a href="../src/a_x.ml.html" class="source_link">Source</a>

`A.X` and `A.X.Y` should contain a link to `A_x.ml.html`:

  $ grep source_link html/A/X/index.html
      <a href="../../src/a_x.ml.html" class="source_link">Source</a>
       <a href="../../src/a_x.ml.html#module-Y" class="source_link">Source</a>
  $ grep source_link html/A/X/Y/index.html
      <a href="../../../src/a_x.ml.html#module-Y" class="source_link">Source
       <a href="../../../src/a_x.ml.html#module-Y.val-z" class="source_link">
