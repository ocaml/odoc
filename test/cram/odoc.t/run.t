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
  $ sherlodoc_index --format=js --db=db.js *.odocl     
  $ du -sh db.js
  16K	db.js
  $ odoc html-generate --with-search --output-dir html main.odocl
  $ odoc html-generate --with-search --output-dir html page-page.odocl
  $ odoc support-files -o html
  $ cat db.js  ../../../bin/JSherlodoc/main.bc.js > html/index.js
  $ du -sh html/index.js
  4.0M	html/index.js
  $ cp -r html /tmp
  $ firefox /tmp/html/Main/index.html

