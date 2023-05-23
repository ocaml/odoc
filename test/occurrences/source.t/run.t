When both source rendering and occurrence counting are enabled, the occurrences information are used to generate "jump to documentation" links.

This test tests this.

Files containing some values:

  $ cat a.ml | head -n 3
  let a = B.b

  $ cat b.ml | head -n 3
  let b = 3

Source pages require a parent:

  $ odoc compile -c module-a -c module-b -c src-source root.mld

Compile the modules:

  $ ocamlc -c b.ml -bin-annot
  $ ocamlc -c a.ml -I . -bin-annot

Compile the pages with the source and occurrences options

  $ printf "a.ml\nb.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map
  $ odoc compile --count-occurrences -I . --source-name b.ml --source-parent-file src-source.odoc b.cmt
  $ odoc compile --count-occurrences -I . --source-name a.ml --source-parent-file src-source.odoc a.cmt
  $ odoc link -I . b.odoc
  $ odoc link -I . a.odoc
  $ odoc html-generate --source b.ml --indent -o html b.odocl
  $ odoc html-generate --source a.ml --indent -o html a.odocl
  $ odoc support-files -o html

The source for `a` contains a link to the documentation of `B.b`, as it is used in the implementation:

  $ cat html/root/source/a.ml.html | tr '> ' '\n\n' | grep 'href' | grep val-b
  href="b.ml.html#val-b"
  href="../../B/index.html#val-b"
