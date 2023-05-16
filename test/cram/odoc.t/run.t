  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile -I . main.cmt
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
$ cp  /home/emile/.opam/sherlodoc/var/cache/odig/odoc/dream/**.odocl .
  $ ls
  main.cmi
  main.cmo
  main.cmt
  main.ml
  main.odoc
  main.odocl
  page-page.odoc
  page-page.odocl
  page.mld
  $ odoc compile-index --binary -I . -o index.odoc_bin
  $ du -sh index.odoc_bin
  8.0K	index.odoc_bin
  $ sherlodoc_index --format=js --odoc=index.odoc_bin  --db=db.js      
  $ du -sh db.js
  20K	db.js
  $ odoc html-generate --with-search --output-dir html main.odocl
  $ odoc html-generate --with-search --output-dir html dream.odocl
  odoc: FILE.odocl argument: no 'dream.odocl' file or directory
  Usage: odoc html-generate [OPTION]… FILE.odocl
  Try 'odoc html-generate --help' or 'odoc --help' for more information.
  [2]
$ odoc html-generate --with-search --output-dir html stdlib.odocl
  $ odoc html-generate --with-search --output-dir html page-page.odocl
  $ odoc support-files -o html
  $ cat db.js  ../../../bin/JSherlodoc/main.bc.js > html/index.js
  $ du -sh html/index.js
  3.8M	html/index.js
  $ cp -r html /tmp
  $ firefox /tmp/html/Main/index.html
