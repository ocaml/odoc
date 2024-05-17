We make a 'package' mld file:

  $ cat package.mld
  {0 Package page}
  
















And we'll have a module that we'll put underneath this package page.

  $ cat test.mli
  type t
  

















  $ ocamlc -c -bin-annot test.mli

  $ odoc compile test.cmti -I . --parent-id package --output-dir .

  $ odoc compile package.mld --parent-id package --output-dir .
  $ odoc compile index.mld --parent-id package --output-dir .
  $ odoc compile test_page.mld --parent-id package/Test --output-dir .



Link and generate the HTML:

  $ ls
  index.mld
  package
  package.mld
  test.cmi
  test.cmti
  test.mli
  test_page.mld
  $ ls package
  Test
  page-index.odoc
  page-package.odoc
  test.odoc
  $ for i in $(find . -name *.odoc); do odoc link -I . $i; done
  $ for i in $(find . -name *.odocl); do odoc html-generate $i -o html; done

We should see a directory structure here where the module 'Test' is found underneath the top-level directory 'package'. Also, the contents of the
file 'package.mld' should be written to the file 'package/index.html'.

  $ find html -type f | sort
  html/package/Test/index.html
  html/package/Test/test_page.html
  html/package/index.html
  html/package/package.html

Let's make sure the manpage and latex renderers work too

  $ for i in $(find . --name *.odocl); do odoc man-generate $i -o man; odoc latex-generate $i -o latex; done
  find: unknown predicate `--name'

  $ find man -type f | sort
  find: 'man': No such file or directory

  $ find latex -type f | sort
  find: 'latex': No such file or directory

