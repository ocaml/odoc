  $ export ODOCLS=$(find ../docs/odoc/base/ -name '*.odocl')
  $ cat $ODOCLS > megaodocl
  $ du -sh megaodocl
  5.4M	megaodocl
  $ sherlodoc index --index-docstring=true --index-name=true --type-search=true --format=js --db=db.js $ODOCLS > /dev/null

  $ gzip -k db.js

We want to compare the compressed size with the size of the odocl. The search
database contains information than the odocl, but the information is organised
in queryable way, so a size increase is expected. It should just be reasonable.

  $ du -s *.js *.gz
  2064	db.js
  1560	db.js.gz

  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --search-uri=db.js --search-uri=sherlodoc.js --output-dir html $f
  > done
  $ odoc support-files -o html
  $ cp db.js html/
The --no-preserve flag is here so that copying to /tmp will not fail because of
a previous run. .js files built by dune are read only.
  $ sherlodoc js html/sherlodoc.js
  $ ls html
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
