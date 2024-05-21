We make a 'package' mld file:

  $ cat package.mld
  {0 Package page}

And we'll have a module that we'll put underneath this package page.

  $ cat test.mli
  type t

  $ ocamlc -c -bin-annot test.mli

  $ odoc compile test.cmti -I . --parent-id package --output-dir .

We do not check that directory given by --parent-id  are indeed directories and
not for instance modules
Here, it is possible to do
$ odoc compile index.mld --parent-id package/Test --output-dir .
without triggering an error, even though this is a conflict where one of the
package/Test/index.html page is overwritten silently.

  $ odoc compile package.mld --parent-id package --output-dir .
  $ odoc compile index.mld --parent-id package --output-dir .
  $ odoc compile test_page.mld --parent-id package/foo --output-dir .

Link and generate the HTML:

  $ ls
  index.mld
  package
  package.mld
  test.cmi
  test.cmti
  test.mli
  test_page.mld
  $ find package -type f | sort
  package/foo/page-test_page.odoc
  package/page-index.odoc
  package/page-package.odoc
  package/test.odoc
  $ for i in $(find . -name *.odoc); do odoc link -I . $i; done
  $ for i in $(find . -name *.odocl); do odoc html-generate $i -o html; done

We should see a directory structure here where the module 'Test' is found underneath the top-level directory 'package'. Also, the contents of the
file 'package.mld' should be written to the file 'package/index.html'.

  $ find html -type f | sort
  html/package/Test/index.html
  html/package/foo/test_page.html
  html/package/index.html
  html/package/package.html

Let's make sure the manpage and latex renderers work too

  $ for i in $(find . -name *.odocl); do odoc man-generate $i -o man; odoc latex-generate $i -o latex; done

  $ find man -type f | sort
  man/package/Test.3o
  man/package/foo/test_page.3o
  man/package/index.3o
  man/package/package.3o

  $ find latex -type f | sort
  latex/package/Test.tex
  latex/package/foo/test_page.tex
  latex/package/index.tex
  latex/package/package.tex

