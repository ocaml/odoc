Similar to Astring library.

  $ odoc compile -c module-a -c src-source root.mld

  $ printf "a.ml\na_x.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c -o a_x.cmo a_x.ml -bin-annot -I .
  $ ocamlc -c a.mli -bin-annot -I .
  $ ocamlc -c a.ml -bin-annot -I .

  $ odoc compile --hidden --source-name a_x.ml --source-parent-file src-source.odoc -I . a_x.cmt
  $ odoc compile --cmt a.cmt --source-name a.ml --source-parent-file src-source.odoc -I . a.cmti

  $ odoc link -I . a_x.odoc
  $ odoc link -I . a.odoc

  $ odoc html-generate --source a_x.ml --indent -o html a_x.odocl
  $ odoc html-generate --source a.ml --indent -o html a.odocl

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
  html/root
  html/root/source
  html/root/source/a.ml.html
  html/root/source/a_x.ml.html

Documentation for `A_x` is not generated for hidden modules:

  $ ! [ -f html/A_x/index.html ]
  [1]

Code source for `A_x` is wanted:

  $ [ -f html/root/source/a_x.ml.html ]

`A` should contain a link to `A_x.ml.html`:

  $ grep source_link html/A/index.html
      <a href="../root/source/a.ml.html" class="source_link">Source</a>
       <a href="../root/source/a_x.ml.html" class="source_link">Source</a>

`A.X` and `A.X.Y` should contain a link to `A_x.ml.html`:

  $ grep source_link html/A/X/index.html
      <a href="../../root/source/a_x.ml.html" class="source_link">Source</a>
       <a href="../../root/source/a_x.ml.html#module-Y" class="source_link">
  $ grep source_link html/A/X/Y/index.html
      <a href="../../../root/source/a_x.ml.html#module-Y" class="source_link">
        class="source_link">Source
