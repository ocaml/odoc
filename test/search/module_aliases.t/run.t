Compile the files

  $ ocamlc -c main.ml -bin-annot -I .

Compile and link the documentation

  $ odoc compile main.cmt
  $ odoc link main.odoc
  $ odoc compile-index --json -P pkgname:.

Search results only redirect to their definition point (not the
expansions). Comments link to the expansion they are in.

  $ cat index.json | jq -r '.[] | "\(.id[-1].name) -> \(.display.url)"'
  Main -> Main/index.html
  X -> Main/X/index.html
  x -> Main/X/index.html#val-x
  X -> Main/X/index.html
  Y -> Main/index.html#module-Y
  Z -> Main/index.html#module-Z
  L -> Main/index.html#module-L
  X -> Main/module-type-X/index.html
  x -> Main/module-type-X/index.html#val-x
  Y -> Main/index.html#module-type-Y
  Z -> Main/index.html#module-type-Z
  L -> Main/index.html#module-type-L
