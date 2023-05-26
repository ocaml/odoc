  $ cd base
  $ cd ..
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  5.1M	megaodocl
  $ sherlodoc_index --format=js  --db=db.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ du -sh *.js
  20M	db.js
  16M	db_empty_payload.js
  17M	db_no_docstring.js
  15M	db_no_name.js
  13M	db_no_type.js
  6.4M	db_only_names.js
  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --with-search --output-dir html $f 2> /dev/null
  > done
  $ odoc support-files -o html
  $ cat db.js  ../../../bin/JSherlodoc/main.bc.js > html/index.js
  $ du -sh html/index.js
  23M	html/index.js
  $ cp -r html /tmp
  $ firefox /tmp/html/index.html
