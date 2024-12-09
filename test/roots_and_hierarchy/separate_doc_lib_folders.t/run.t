The hierarchy tested in this file is to put pages in `pkgname/doc/` and
libraries in `pkgname/lib/libraryname/`.

No name clashes can happen, but the generated hierarchy is less natural. Moreover, we need the `--current-package` for `{!//index}` references to work.

  $ ocamlc -c -bin-annot unit.ml

Let's generate the hierarchy:

  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/lib/libname unit.cmt

Let's link it:

  $ odoc link -P pkg:_odoc/pkg/doc/ -L libname:_odoc/pkg/lib/libname _odoc/pkg/doc/page-file.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/dir1/page-my_page.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/dir1/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/lib/libname/unit.odoc --current-package pkg

Let's html-generate it (with a sidebar):

  $ odoc compile-index --root _odoc/pkg/doc/ --root _odoc/pkg/lib/libname
  $ odoc sidebar-generate index.odoc-index

  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/doc/page-file.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/doc/dir1/page-my_page.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/doc/dir1/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/doc/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/lib/libname/unit.odocl

Now, let's see the result

  $ find  html -name *.html | sort
  html/pkg/doc/dir1/index.html
  html/pkg/doc/dir1/my_page.html
  html/pkg/doc/file.html
  html/pkg/doc/index.html
  html/pkg/lib/libname/Unit/index.html

