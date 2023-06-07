  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile -I . main.cmt
  $ odoc compile -I . page.mld
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  8.0K	megaodocl
  $ sherlodoc_index --format=js --db=db.js $(find . -name '*.odocl') 2> /dev/null
  Index_lib.main
  loading doc !
  doc loaded
  $ sherlodoc_index --format=marshal --db=sherlodoc_db.bin $(find . -name '*.odocl') 2> /dev/null
  Index_lib.main
  loading doc !
  doc loaded
$ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ gzip -k db.js

  $ gzip -k megaodocl

  $ du -s *.js *.gz
  8	db.js
  4	db.js.gz
  4	megaodocl.gz


  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --with-search --output-dir html $f 2> /dev/null
  > done
  $ odoc support-files -o html
  $ cat db.js  ../../../jsoo/main.bc.js > html/index.js
  $ cp sherlodoc_db.bin html
  $ du -sh html/index.js
  4.9M	html/index.js
  $ ls html
  Main
  fonts
  highlight.pack.js
  index.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  page.html
  sherlodoc_db.bin
  $ cp -r html /tmp
  $ firefox /tmp/html/Main/index.html

