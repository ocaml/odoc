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
  File "file.mld", line 3, characters 0-20:
  Warning: Failed to resolve reference /pkg/dir1/my_page Path '/pkg/dir1/my_page' not found

Testing missing file:
  $ rm _odoc/pkg/doc/dir1/page-my_page.odoc*
  $ odoc link -P pkg:_odoc/pkg/doc/ _odoc/pkg/doc/page-file.odoc
  File "file.mld", line 3, characters 0-20:
  Warning: Failed to resolve reference /pkg/dir1/my_page Path '/pkg/dir1/my_page' not found
