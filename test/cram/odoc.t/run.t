  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile -I . main.cmt
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
$ cp /home/emile/.opam/sherlodoc/var/cache/odig/odoc/**/*.odocl .
  $ odoc compile-index --binary -I . -o index.odoc_bin
  $ du -sh index.odoc_bin
  4.0K	index.odoc_bin
  $ sherlodoc_index --format=js --odoc=index.odoc_bin  --db=db.js      
  $ du -sh db.js
  12K	db.js
  $ odoc html-generate --with-search --output-dir html main.odocl
$ odoc html-generate --with-search --output-dir html stdlib.odocl
  $ odoc html-generate --with-search --output-dir html page-page.odocl
  $ odoc support-files -o html
  $ cat db.js  ../../../bin/JSherlodoc/main.bc.js > html/index.js
  $ cp -r html /tmp
  $ firefox /tmp/html/Main/index.html
