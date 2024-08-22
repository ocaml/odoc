Compile the modules:

  $ ocamlc -c lib/a/a.ml -bin-annot
  $ ocamlc -c lib/b/b.ml -bin-annot
  $ ocamlc -c lib/main.ml -bin-annot

Now, compile the pages with the --source option. The source-name must be included in the source-children of the source-parent:

  $ odoc compile-impl -I . --source-id lib/a/a.ml lib/a/a.cmt
  $ odoc compile -I . lib/a/a.cmt -o a.odoc
  $ odoc compile-impl -I . --source-id lib/b/b.ml lib/b/b.cmt
  $ odoc compile -I . lib/b/b.cmt -o b.odoc
  $ odoc compile-impl -I . --source-id lib/main.ml lib/main.cmt
  $ odoc compile -I . lib/main.cmt -o main.odoc
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc link -I . main.odoc
  $ odoc link -I . impl-a.odoc
  $ odoc link -I . impl-b.odoc
  $ odoc link -I . impl-main.odoc
  $ odoc html-generate-source --impl impl-a.odocl --indent -o html lib/a/a.ml
  $ odoc html-generate --indent -o html a.odocl
  $ odoc html-generate-source --impl impl-b.odocl --indent -o html lib/b/b.ml
  $ odoc html-generate --indent -o html b.odocl
  $ odoc html-generate --indent -o html main.odocl
  $ odoc html-generate-source --impl impl-main.odocl --indent -o html lib/main.ml

Source pages and source directory pages are generated:

  $ find html | sort
  html
  html/A
  html/A/index.html
  html/B
  html/B/index.html
  html/Main
  html/Main/index.html
  html/lib
  html/lib/a
  html/lib/a/a.ml.html
  html/lib/b
  html/lib/b/b.ml.html
  html/lib/main.ml.html
