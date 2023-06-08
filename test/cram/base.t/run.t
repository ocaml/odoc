  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  5.1M	megaodocl
  $ time sherlodoc_index --format=js --db=db.js $(find . -name '*.odocl') 2> /dev/null
  Index_lib.main
  Indexing in 9.149565s
  trie_with_array_occ:0.35s
  trie_with_array:0.26s
  Cache.Elt_array_occ_trie.memo:2.40s
  Cache.Elt_array_trie.memo:2.17s
  
  real	0m15.614s
  user	0m15.322s
  sys	0m0.237s
$ sherlodoc_index --format=marshal --db=db_marshal.bin $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ gzip -k db.js

  $ gzip -k megaodocl

  $ du -s *.js *.gz
  2972	db.js
  2240	db.js.gz
  1628	megaodocl.gz


  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --with-search --output-dir html $f 2> /dev/null
  > done
  $ odoc support-files -o html
  $ cat db.js  ../../../jsoo/main.bc.js > html/index.js
  $ cp sherlodoc_db.bin html
  cp: cannot stat 'sherlodoc_db.bin': No such file or directory
  [1]
  $ du -sh html/index.js
  15M	html/index.js
  $ ls html
  base
  fonts
  highlight.pack.js
  index.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  $ cp -r html /tmp
  $ firefox /tmp/html/base/index.html
