  $ find . -name '*.odocl' | sort
  ./base_odocls/base.odocl
  ./base_odocls/base_internalhash_types.odocl
  ./base_odocls/caml.odocl
  ./base_odocls/md5_lib.odocl
  ./base_odocls/page-index.odocl
  ./base_odocls/shadow_stdlib.odocl
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  4.8M	megaodocl
  $ sherlodoc index --index-docstring=true --index-name=true --type-search=true --format=js --db=db.js $(find . -name '*.odocl')

  $ gzip -k db.js

We want to compare the compressed size with the size of the odocl. The search
database contains information than the odocl, but the information is organised
in queryable way, so a size increase is expected. It should just be reasonable.
  $ gzip -k megaodocl

  $ du -s *.js *.gz
  2220	db.js
  1676	db.js.gz
  1548	megaodocl.gz

  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --search-uri=db.js --search-uri=sherlodoc.js --output-dir html $f
  > done
  $ odoc support-files -o html
  $ cp db.js html/
The --no-preserve flag is here so that copying to /tmp will not fail because of
a previous run. .js files built by dune are read only.
  $ cp --no-preserve=mode,ownership ../../jsoo/main.bc.js html/sherlodoc.js
  $ du -sh html/sherlodoc.js
  92K	html/sherlodoc.js
  $ ls html
  base
  db.js
  fonts
  highlight.pack.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  sherlodoc.js
indent to see results
$ cp -r html /tmp
$ firefox /tmp/html/base/index.html
