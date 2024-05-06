Compile the files

  $ ocamlc -c main.ml -bin-annot -I .

Compile and link the documentation

  $ odoc compile -I . main.cmt
  $ odoc link -I . main.odoc

  $ odoc compile-index main.odocl

Let's have a look at the links generated for standalone comments search entries:

  $ cat index.json | jq -r '.[] | select(.kind.kind | contains("Doc")) | "\(.doc) -> \(.display.url)"'
  standalone 1 -> Main/index.html
  standalone 2 -> Main/X/index.html
  standalone 3 -> Main/X/index.html

There is a problem with "standalone 3": it should link to Main/index.html, not Main/X/index.html
This is a bug!
