  $ ocamlc -c -bin-annot unit.ml

  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/my_page.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/dir1 dir1/index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg file.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg index.mld
  $ odoc compile --output-dir _odoc/ --parent-id pkg/libname unit.cmt

  $ odoc link _odoc/pkg/page-file.odoc 2>&1 >/dev/null | grep 'Failure'
  [1]
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/page-file.odoc

Testing the collision detection:

Same directory used twice
  $ odoc link -P pkg:_odoc/pkg -P pkg2:_odoc/pkg _odoc/pkg/page-file.odoc
  ERROR: Paths given to all -P options must be disjoint
  [1]

# Two directories given relatively
 Right input:
  $ mkdir _odoc/pkg2
  $ odoc link -P pkg:_odoc/pkg/ -P pkg2:_odoc/pkg2/ _odoc/pkg/page-file.odoc
 Wrong input:
  $ odoc link -P pkg:_odoc/pkg -P pkg2:_odoc/pkg _odoc/pkg/page-file.odoc
  ERROR: Paths given to all -P options must be disjoint
  [1]

# Two directories given relatively with -L
 Right input:
  $ mkdir _odoc/mylib
  $ odoc link -P pkg:_odoc/pkg/ -L mylib:_odoc/mylib/ _odoc/pkg/page-file.odoc
 Also right input (-L does not exclude its content from -P roots):
  $ odoc link -P pkg:_odoc/pkg -L pkg2:_odoc/pkg _odoc/pkg/page-file.odoc

# One directory given relatively, the other absolutely
 Right input
  $ odoc link -P pkg:_odoc/pkg/ -P pkg2:$PWD/_odoc/pkg2 _odoc/pkg/page-file.odoc
 Wrong input
  $ odoc link -P pkg:_odoc/pkg/ -P pkg2:$PWD/_odoc/pkg _odoc/pkg/page-file.odoc
  ERROR: Paths given to all -P options must be disjoint
  [1]

# Two directories given absolutely
 Right input
  $ odoc link -P pkg:$PWD/_odoc/pkg/ -P pkg2:$PWD/_odoc/pkg2 _odoc/pkg/page-file.odoc
 Wrong input
  $ odoc link -P pkg:$PWD/_odoc/pkg/ -P pkg2:$PWD/_odoc/pkg _odoc/pkg/page-file.odoc
  ERROR: Paths given to all -P options must be disjoint
  [1]

# With a bit of relative faff
 Right input:
  $ odoc link -P pkg:_odoc/../_odoc/pkg/ -P pkg2:_odoc/../_odoc/pkg2 _odoc/pkg/page-file.odoc
 Wrong input:
  $ odoc link -P pkg:_odoc/../_odoc/pkg/ -P pkg2:_odoc/../_odoc/pkg _odoc/pkg/page-file.odoc
  ERROR: Paths given to all -P options must be disjoint
  [1]

Testing detection of package:
 Can detect:
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/page-file.odoc

Testing missing file:
  $ rm _odoc/pkg/dir1/page-my_page.odoc*
  $ odoc link -P pkg:_odoc/pkg/ _odoc/pkg/page-file.odoc
  File "file.mld", line 3, characters 0-20:
  Warning: Failed to resolve reference /pkg/dir1/my_page Path '/pkg/dir1/my_page' not found
