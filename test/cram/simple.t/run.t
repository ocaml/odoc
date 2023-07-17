  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile --child asset-db.js --child asset-sherlodoc.js --child module-main --search-asset=db.js --search-asset=sherlodoc.js -I . page.mld
  $ odoc compile --parent page --search-asset=db.js --search-asset=sherlodoc.js -I . main.cmt
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  12K	megaodocl
  $ sherlodoc_index --format=js --db=db.js $(find . -name '*.odocl') 2> /dev/null
  Indexing in 1.182795ms
  Export in 0.593901ms

Here cat is used to remove weird permissions on executable built by dune
  $ cat ../../../jsoo/main.bc.js > sherlodoc.js
  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --asset db.js --asset sherlodoc.js --output-dir html $f 2> /dev/null
  > done
  $ odoc support-files -o html
  $ ls html
  fonts
  highlight.pack.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  page
  $ cp -r html /tmp
  $ firefox /tmp/html/Main/index.html

