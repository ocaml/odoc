  $ odoc compile my_page.mld
  $ mkdir -p a/b/c/
  $ cp page-my_page.odoc a/b/c/

  $ odoc compile file.mld

  $ odoc link -P pkg:a/ page-file.odoc
  Finding by name: page-my_page.odoc
  First visit of pkg: a/
  Encountering a/b/c/page-my_page.odoc: adding page-my_page.odoc
