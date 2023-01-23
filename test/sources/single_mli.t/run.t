Similar to Astring library.

  $ ocamlc -c -o a_x.cmo a_x.ml -bin-annot -I .
  $ ocamlc -c a.mli -bin-annot -I .
  $ ocamlc -c a.ml -bin-annot -I .

  $ odoc compile --hidden --impl a_x.ml -I . a_x.cmt
  $ odoc compile --impl a.ml -I . a.cmti

  $ odoc link -I . a_x.odoc
  $ odoc link -I . a.odoc

TODO: It seems that --hidden do not work:
  $ odoc_print a_x.odoc | grep hidden
    "hidden": "false",
                "hidden": "false"
  $ odoc_print a_x.odocl | grep hidden
    "hidden": "false",

  $ odoc html-generate --indent -o html a_x.odocl
  $ odoc html-generate --indent -o html a.odocl

Look if all the source files are generated:

  $ find html | sort
  html
  html/A
  html/A/A.ml.html
  html/A/X
  html/A/X/Y
  html/A/X/Y/index.html
  html/A/X/index.html
  html/A/index.html
  html/A_x
  html/A_x/A_x.ml.html
  html/A_x/index.html

Documentation for `A_x` is not generated for hidden modules, but --hidden do not
work right now:

  $ ! [ -f html/A_x/index.html ]
  [1]

Code source for `A_x` is wanted:

  $ [ -f html/A_x/A_x.ml.html ]

`A` should contain a link to `A_x.ml.html`:

  $ grep source_link html/A/index.html
      <a href="A.ml.html" class="source_link">Source</a>
       <a href="../A_x/A_x.ml.html#compunit-A_x" class="source_link">Source</a>

`A.X` and `A.X.Y` should contain a link to `A_x.ml.html`:

  $ grep source_link html/A/X/index.html
      <a href="../../A_x/A_x.ml.html#compunit-A_x" class="source_link">Source
       <a href="../../A_x/A_x.ml.html#def-A_x1" class="source_link">Source</a>
  $ grep source_link html/A/X/Y/index.html
      <a href="../../../A_x/A_x.ml.html#def-A_x1" class="source_link">Source
       <a href="../../../A_x/A_x.ml.html#def-A_x0" class="source_link">Source
