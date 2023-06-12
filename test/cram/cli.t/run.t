  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile -I . main.cmt
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  8.0K	megaodocl
  $ sherlodoc_index --format=marshal --db=db.bin $(find . -name '*.odocl') 2> /dev/null > /dev/null
  $ sherlodoc --db=db.bin "lorem"
  val Main.lorem : 'a -> char
  val Main.lorem4 : int
  val Main.lorem2 : 'a -> char
  val Main.lorem3 : 'a -> char
