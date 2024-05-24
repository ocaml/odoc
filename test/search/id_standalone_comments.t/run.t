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
  standalone 3 -> Main/index.html

The entries link to the pages that contain the standalone comment (they do not
have an ID, so they cannot be linked directly).
