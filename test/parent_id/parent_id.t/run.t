  $ ocamlc -c -bin-annot unit.ml

  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/doc index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/lib/libname unit.cmt

  $ odoc link _odoc/pkg/doc/page-file.odoc 2>&1 >/dev/null | grep 'Failure'
  [1]
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/page-file.odoc

Testing the collision detection:

Same directory used twice
  $ odoc link -P pkg:_odoc/pkg/doc -P pkg2:_odoc/pkg/doc _odoc/pkg/doc/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

# Two directories given relatively
 Right input:
  $ mkdir _odoc/pkg2
  $ odoc link -P pkg:_odoc/pkg/doc/ -P pkg2:_odoc/pkg2/ _odoc/pkg/doc/page-file.odoc
 Wrong input:
  $ odoc link -P pkg:_odoc/pkg/doc -P pkg2:_odoc/pkg _odoc/pkg/doc/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

# Two directories given relatively with -L
 Right input:
  $ mkdir _odoc/mylib
  $ odoc link -P pkg:_odoc/pkg/doc/ -L mylib:_odoc/mylib/ _odoc/pkg/doc/page-file.odoc
 Wrong input:
  $ odoc link -P pkg:_odoc/pkg/doc -L pkg2:_odoc/pkg _odoc/pkg/doc/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

# One directory given relatively, the other absolutely
 Right input
  $ odoc link -P pkg:_odoc/pkg/doc/ -P pkg2:$PWD/_odoc/pkg2 _odoc/pkg/doc/page-file.odoc
 Wrong input
  $ odoc link -P pkg:_odoc/pkg/doc/ -P pkg2:$PWD/_odoc/pkg _odoc/pkg/doc/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

# Two directories given absolutely
 Right input
  $ odoc link -P pkg:$PWD/_odoc/pkg/doc/ -P pkg2:$PWD/_odoc/pkg2 _odoc/pkg/doc/page-file.odoc
 Wrong input
  $ odoc link -P pkg:$PWD/_odoc/pkg/doc/ -P pkg2:$PWD/_odoc/pkg _odoc/pkg/doc/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

# With a bit of relative faff
 Right input:
  $ odoc link -P pkg:_odoc/../_odoc/pkg/doc/ -P pkg2:_odoc/../_odoc/pkg2 _odoc/pkg/doc/page-file.odoc
 Wrong input:
  $ odoc link -P pkg:_odoc/../_odoc/pkg/doc/ -P pkg2:_odoc/../_odoc/pkg _odoc/pkg/doc/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

Testing detection of package:
 Can detect:
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/page-file.odoc
 Cannot detect due to wrong input:
  $ odoc link -P pkg2:_odoc/pkg/doc/ _odoc/pkg/doc/page-file.odoc
  Not found by nameError during find by path: no package was found with this name
  File "file.mld", line 4, characters 0-28:
  Warning: Failed to resolve reference unresolvedroot(#/pkg/dir1/my_page) Couldn't find page "#/pkg/dir1/my_page"
  File "file.mld", line 3, characters 0-18:
  Warning: Failed to resolve reference unresolvedroot(#my_page) Couldn't find page "#my_page"

Testing missing file:
  $ rm _odoc/pkg/doc/dir1/page-my_page.odoc*
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/page-file.odoc
  Page not found by name: 0
  Error during find by path: no file was found with this path: dir1/page-my_page.odoc
  File "file.mld", line 4, characters 0-28:
  Warning: Failed to resolve reference unresolvedroot(#/pkg/dir1/my_page) Couldn't find page "#/pkg/dir1/my_page"
  File "file.mld", line 3, characters 0-18:
  Warning: Failed to resolve reference unresolvedroot(#my_page) Couldn't find page "#my_page"
