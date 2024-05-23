  $ odoc compile my_page.mld
  $ mkdir -p a/b/c/
  $ mkdir -p z
  $ cp page-my_page.odoc a/b/c/
  $ cp file.mld a/

  $ odoc compile a/file.mld

  $ odoc link a/page-file.odoc 2>&1 >/dev/null | grep 'Failure'
        Failure("Not found by name")
  $ odoc link -P pkg:a/ a/page-file.odoc

Testing the collision detection:

Same directory used twice
  $ odoc link -P pkg:a/ -P pkg2:a/ a/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

# Two directories given relatively
 Right input:
  $ odoc link -P pkg:a/ -P pkg2:z/ a/page-file.odoc
 Wrong input:
  $ odoc link -P pkg:a/ -P pkg2:a/b a/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

# One directory given relatively, the other absolutely
 Right input
  $ odoc link -P pkg:a/ -P pkg2:$PWD/z/ a/page-file.odoc
 Wrong input
  $ odoc link -P pkg:a/ -P pkg2:$PWD/a/b page-file.odoc
  odoc: FILE.odoc argument: no 'page-file.odoc' file or directory
  Usage: odoc link [--open=MODULE] [OPTION]… FILE.odoc
  Try 'odoc link --help' or 'odoc --help' for more information.
  [2]

# Two directories given absolutely
 Right input
  $ odoc link -P pkg:$PWD/a/ -P pkg2:$PWD/z/ a/page-file.odoc
 Wrong input
  $ odoc link -P pkg:$PWD/a/ -P pkg2:$PWD/a/b a/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

# With a bit of relative faff
 Right input:
  $ odoc link -P pkg:a/../a -P pkg2:z/../z a/page-file.odoc
 Wrong input:
  $ odoc link -P pkg:a/../a -P pkg2:z/../a/b a/page-file.odoc
  ERROR: Arguments given to -P and -L cannot be included in each others
  [1]

Testing detection of package:
  $ mkdir alpha
  $ cp a/page-file.odoc alpha/page-file.odoc
  $ odoc link -P pkg:a/ -P alpha:alpha alpha/page-file.odoc
  $ odoc link -P pkg:a/  alpha/page-file.odoc
  ERROR: The output file must be part of a directory passed as a -P or -L
  [1]
