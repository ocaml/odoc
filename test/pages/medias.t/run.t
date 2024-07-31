We need to odoc-compile the package mld file, listing its children

  $ odoc compile index.mld --parent-id pkg1/doc/ --output-dir _odoc

  $ odoc compile-asset --parent-id pkg1/doc/ --output-dir _odoc --name caml.gif

This will have produced a file called 'page-index.odoc'.

Link (and generate the HTML):
  $ odoc link -P pkg1:_odoc/pkg1/doc _odoc/pkg1/doc/page-index.odoc
  File "index.mld", line 33, characters 48-64:
  Warning: Failed to resolve reference ./module-x Path 'module-x' not found
  File "index.mld", line 12, characters 28-83:
  Warning: Failed to resolve reference ./camezfzeffl.gif Path 'camezfzeffl.gif' not found
  File "index.mld", line 11, characters 31-53:
  Warning: Failed to resolve reference ./caqzdqzdml.gif Path 'caqzdqzdml.gif' not found
  $ odoc html-generate -o html --indent _odoc/pkg1/doc/page-index.odocl
  $ odoc support-files -o html

To test visually, indent:
 $ cp -r html /tmp/
 $ firefox /tmp/html/index/index.html

Testing the working references:

  $ cat html/index/index.html | grep img
  cat: html/index/index.html: No such file or directory
  [1]

  $ cat html/index/index.html | grep video
  cat: html/index/index.html: No such file or directory
  [1]

  $ cat html/index/index.html | grep audio
  cat: html/index/index.html: No such file or directory
  [1]

Testing the unresolved references:

  $ cat html/index/index.html | grep xref-unresolved
  cat: html/index/index.html: No such file or directory
  [1]

Testing latex and manpages

  $ odoc latex-generate -o latex page-index.odocl
  odoc: FILE.odocl argument: no 'page-index.odocl' file or directory
  Usage: odoc latex-generate [OPTION]… FILE.odocl
  Try 'odoc latex-generate --help' or 'odoc --help' for more information.
  [2]
  $ cat latex/index.tex | grep ocamlinlinecode
  cat: latex/index.tex: No such file or directory
  [1]

  $ odoc man-generate -o man page-index.odocl
  odoc: FILE.odocl argument: no 'page-index.odocl' file or directory
  Usage: odoc man-generate [OPTION]… FILE.odocl
  Try 'odoc man-generate --help' or 'odoc --help' for more information.
  [2]
  $ cat man/index.3o | grep gif
  cat: man/index.3o: No such file or directory
  [1]
  $ cat man/index.3o | grep "With alt text"
  cat: man/index.3o: No such file or directory
  [1]
