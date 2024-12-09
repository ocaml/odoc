Compile the files

  $ ocamlc -c main.ml -bin-annot -I .

Compile and link the documentation

  $ odoc compile -I . main.cmt
  $ odoc link -I . main.odoc

  $ odoc compile-index --json --root ./
We test that you can also pass a .odocl file directly.
  $ odoc compile-index --json main.odocl -o index2.json
Indexes should be the same no matter how the inputs were passed.
  $ diff index.json index2.json

Let's have a look at the links generated for standalone comments search entries:

  $ cat index.json | jq -r '.[] | select(.kind.kind | contains("Doc")) | "\(.doc) -> \(.display.url)"'
  standalone 1 -> Main/index.html
  standalone 2 -> Main/X/index.html
  standalone 3 -> Main/index.html

The entries link to the pages that contain the standalone comment (they do not
have an ID, so they cannot be linked directly).
