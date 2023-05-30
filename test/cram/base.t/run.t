  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  5.1M	megaodocl
  $ sherlodoc_index --format=js  --db=db.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ gzip -k db.js
  $ gzip -k megaodocl

  $ du -s *.js *.gz
  8424	db.js
  8424	db_empty_payload.js
  7904	db_no_docstring.js
  5744	db_no_name.js
  3168	db_no_type.js
  2648	db_only_names.js
  1288	db.js.gz
  1628	megaodocl.gz

  $ du -sh *.js *.gz
  8.3M	db.js
  8.3M	db_empty_payload.js
  7.8M	db_no_docstring.js
  5.7M	db_no_name.js
  3.1M	db_no_type.js
  2.6M	db_only_names.js
  1.3M	db.js.gz
  1.6M	megaodocl.gz
  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --with-search --output-dir html $f 2> /dev/null
  > done
  $ odoc support-files -o html
  $ cat db.js  ../../../bin/JSherlodoc/main.bc.js > html/index.js
  $ du -sh html/index.js
  13M	html/index.js
  $ cp -r html /tmp
  $ firefox /tmp/html/index.html
