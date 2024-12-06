The "canonical" hierarchy is to put pages in `pkgname/` and libraries in
`pkgname/libraryname/`.

While it can create name clashes, the generated hierarchy is more natural.

  $ ocamlc -c -bin-annot unit.ml

Let's generate the hierarchy:

  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/libname unit.cmt

Let's link it:

  $ odoc link -P pkg:_odoc/pkg/ -L libname:_odoc/pkg/libname _odoc/pkg/page-file.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-my_page.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/dir1/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/page-index.odoc
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/libname/unit.odoc

Let's html-generate it (with a sidebar):

  $ odoc compile-index --root _odoc/pkg/
  $ odoc sidebar-generate index.odoc-index

  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/page-file.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/dir1/page-my_page.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/dir1/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/page-index.odocl
  $ odoc html-generate --indent --sidebar sidebar.odoc-sidebar -o html _odoc/pkg/libname/unit.odocl

Now, let's see the result

  $ find  html -name *.html | sort
  html/pkg/dir1/index.html
  html/pkg/dir1/my_page.html
  html/pkg/file.html
  html/pkg/index.html
  html/pkg/libname/Unit/index.html
