  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  5.1M	megaodocl
  $ time sherlodoc_index --format=js --db=db.js $(find . -name '*.odocl')
  Warning, resolved hidden path: Base__.Int63_emul.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Set_intf.Named.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Int63_emul.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Hash_set_intf.M_sexp_grammar
  Warning, resolved hidden path: Base__.Hash_set_intf.M_sexp_grammar
  Warning, resolved hidden path: {For_generated_code}1.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Warning, resolved hidden path: Base__.Either0.t
  Indexing in 658.329010ms
  Export in 614.521980ms
  1.60user 0.05system 0:01.65elapsed 100%CPU (0avgtext+0avgdata 190028maxresident)k
  0inputs+6408outputs (0major+52927minor)pagefaults 0swaps
$ sherlodoc_index --format=marshal --db=db_marshal.bin $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --empty-payload --db=db_empty_payload.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-docstring=false --db=db_no_docstring.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --index-name=false --db=db_no_name.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --db=db_no_type.js $(find . -name '*.odocl') 2> /dev/null
$ sherlodoc_index --format=js --type-search=false --empty-payload --index-docstring=false  --db=db_only_names.js $(find . -name '*.odocl') 2> /dev/null

  $ gzip -k db.js

  $ gzip -k megaodocl

  $ du -s *.js *.gz
  3200	db.js
  2412	db.js.gz
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
