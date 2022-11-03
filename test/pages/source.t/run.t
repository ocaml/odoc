Files containing some values:

  $ cat a.ml
  let x = 2
  let y = x + 1
  let z = x + y
  $ cat a.mli
  val x : int
  val y : int
  val z : int

Compile the modules:

  $ ocamlc -c a.mli -bin-annot

Compile the pages:

  $ odoc compile a.cmti
  File "a.cmti":
  Warning: No implementation file found for the given interface
  $ for i in *.odoc; do odoc link -I . $i; done
  $ odoc html --impl a.ml --intf a.mli -o html a.odoc

Check the generated pages:

  $ find html -type f | sort
  html/A/index.html
  html/a.ml
  html/a.mli

  $ tidy -q -w 160 -ashtml -utf8 html/a.ml
  tidy: command not found
  [127]

  $ tidy -q -w 160 -ashtml -utf8 html/a.mli
  tidy: command not found
  [127]
