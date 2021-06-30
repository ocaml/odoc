Normally when outputting HTML, the resulting files are organised into a
directory tree based on the modules, module types, classes and class types,
with each filename being 'index.html'. These are organised such that they
are placed underneath the hierarchy of pages and parent pages that the
modules have been 'odoc compiled' with.

In flat mode, we still use the pages to organise the output, but within that
page directory, there are no further directories, and the output is simply
files named similarly to how the directory structure would have been. So
for example, where we normally have

foo/Test/index.html

we would now have

foo/Test.html

  $ ocamlc -c -bin-annot test.mli
  $ odoc compile --package foo test.cmti
  $ odoc link test.odoc
  $ odoc html-generate test.odocl -o html
  $ find html -name "*" | sort
  html
  html/foo
  html/foo/Test
  html/foo/Test/M
  html/foo/Test/M/index.html
  html/foo/Test/M/module-type-N
  html/foo/Test/M/module-type-N/index.html
  html/foo/Test/class-foo
  html/foo/Test/class-foo/index.html
  html/foo/Test/class-type-t
  html/foo/Test/class-type-t/index.html
  html/foo/Test/index.html

  $ odoc html-generate test.odocl -o html_flat --flat
  $ find html_flat -name "*" | sort
  html_flat
  html_flat/foo
  html_flat/foo/Test-M-module-type-N.html
  html_flat/foo/Test-M.html
  html_flat/foo/Test-class-foo.html
  html_flat/foo/Test-class-type-t.html
  html_flat/foo/Test.html
