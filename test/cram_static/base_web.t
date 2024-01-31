  $ mkdir docs
Generating odocls for base with odig. This might give an error on some
dependencies so we do not display error (one was encountered with yojson)
  $ odig odoc --cache-dir=docs base 2> /dev/null
  Updating documentation, this may take some time...
  $ cat $(find ./docs/odoc/base/ -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  6.2M	megaodocl
  $ sherlodoc index --index-docstring=true --index-name=true --type-search=true --format=js --db=db.js $(find ./docs/odoc/base/ -name '*.odocl') > /dev/null

  $ gzip -k db.js

  $ du -s *.js *.gz
  2108	db.js
  1592	db.js.gz

  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --search-uri=db.js --search-uri=sherlodoc.js --output-dir html $f
  > done
  $ odoc support-files -o html
  $ cp db.js html/
The --no-preserve flag is here so that copying to /tmp will not fail because of
a previous run. .js files built by dune are read only.
  $ sherlodoc js html/sherlodoc.js
  $ ls html
  base
  db.js
  fonts
  highlight.pack.js
  katex.min.css
  katex.min.js
  ocaml
  odoc.css
  odoc_search.js
  sexplib0
  sherlodoc.js
indent to see results
$ cp -r html /tmp
$ firefox /tmp/html/base/index.html
