Compile the modules:

  $ ocamlc -c a.ml -bin-annot
  $ ocamlc -c b.ml -bin-annot
  $ ocamlc -c c.ml -bin-annot

Now, compile the pages with the --source option. The source-name must be included in the source-children of the source-parent:

  $ odoc compile-impl -I . --source-id lib/a/a.ml a.cmt
  $ odoc compile -I . a.cmt
  $ odoc compile-impl -I . --source-id lib/b/b.ml b.cmt
  $ odoc compile -I . b.cmt
  $ odoc compile-impl -I . --source-id lib/main.ml c.cmt
  $ odoc compile -I . c.cmt
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc link -I . c.odoc
  $ odoc link -I . impl-a.odoc
  $ odoc link -I . impl-b.odoc
  $ odoc link -I . impl-c.odoc
  $ odoc html-generate-source --impl impl-a.odocl --indent -o html a.ml
  $ odoc html-generate --indent -o html a.odocl
  $ odoc html-generate-source --impl impl-b.odocl --indent -o html b.ml
  $ odoc html-generate --indent -o html b.odocl
  $ odoc html-generate-source --impl impl-c.odocl --indent -o html c.ml
  $ odoc html-generate --indent -o html c.odocl

Source pages and source directory pages are generated:

  $ find html | sort
  html
  html/A
  html/A/index.html
  html/B
  html/B/index.html
  html/C
  html/C/index.html
  html/lib
  html/lib/a
  html/lib/a/a.ml.html
  html/lib/b
  html/lib/b/b.ml.html
  html/lib/main.ml.html

