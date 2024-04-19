  $ ocamlc -bin-annot file.ml
  $ odoc compile file.cmt
  $ odoc link file.odoc
  $ odoc html-generate --indent -o html file.odocl

Module X should inherit from X__Hidden's top comment:

  $ cat html/File/index.html | grep -B 10 X__Hidden
  [1]
