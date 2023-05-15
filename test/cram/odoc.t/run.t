  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile -I . main.cmt
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cp  /home/emile/.opam/sherlodoc/var/cache/odig/odoc/dream/**.odocl .
  $ ls
  dream.odocl
  main.cmi
  main.cmo
  main.cmt
  main.ml
  main.odoc
  main.odocl
  page-index.odocl
  page-page.odoc
  page-page.odocl
  page.mld
  $ odoc compile-index --binary -I . -o index.odoc_bin
  $ du -sh index.odoc_bin
  488K	index.odoc_bin
  $ sherlodoc_index --format=js --odoc=index.odoc_bin  --db=db.js      
  $ du -sh db.js
  868K	db.js
  $ odoc html-generate --with-search --output-dir html main.odocl
  $ odoc html-generate --with-search --output-dir html dream.odocl
$ odoc html-generate --with-search --output-dir html stdlib.odocl
  $ odoc html-generate --with-search --output-dir html page-page.odocl
  $ odoc support-files -o html
  $ cat db.js  ../../../bin/JSherlodoc/main.bc.js > html/index.js
  $ du -sh html/index.js
  1.1M	html/index.js
  $ cp -r html /tmp
  $ firefox /tmp/html/Main/index.html
