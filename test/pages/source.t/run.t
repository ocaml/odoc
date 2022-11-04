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
  $ for i in *.odoc; do odoc link -I . $i; done
  $ odoc html --impl a.ml --intf a.mli --indent -o html a.odoc
  odoc: unknown option '--impl'.
        unknown option '--intf'.
  Usage: odoc html [OPTION]… FILE.odoc
  Try 'odoc html --help' or 'odoc --help' for more information.
  [2]

Check the generated pages:

  $ find html -type f | sort
  find: 'html': No such file or directory

  $ cat html/a.ml
  cat: html/a.ml: No such file or directory
  [1]

  $ cat html/a.mli
  cat: html/a.mli: No such file or directory
  [1]
