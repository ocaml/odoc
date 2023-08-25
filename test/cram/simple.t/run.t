  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile --child asset-db.js --child asset-sherlodoc.js --child module-main --search-asset=db.js --search-asset=sherlodoc.js -I . page.mld
  $ odoc compile --parent page --search-asset=db.js --search-asset=sherlodoc.js -I . main.cmt
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  12K	megaodocl
  $ sherlodoc_index --format=js --db=db.js $(find . -name '*.odocl') 2> /dev/null
  Indexing in 1.058102ms
  Export in 0.505924ms

Here cat is used to remove weird permissions on executable built by dune
  $ cat ../../../jsoo/main.bc.js > sherlodoc.js
  $ odoc support-files -o html
  $ for f in $(find . -name '*.odocl'); do
  >  echo $f ;
  >  odoc html-generate --asset db.js --asset sherlodoc.js --output-dir html $f
  > done
  ./page-page.odocl
  ./main.odocl
  $ ls
  db.js
  html
  main.cmi
  main.cmo
  main.cmt
  main.ml
  main.odoc
  main.odocl
  megaodocl
  page-page.odoc
  page-page.odocl
  page.mld
  sherlodoc.js
  $ ls html
  fonts
  highlight.pack.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  page
  $ ls html/page
  Main
  db.js
  index.html
  sherlodoc.js
  $ find  .html -type f | sort
  find: '.html': No such file or directory
  $ cp -r html /tmp
  $ cp sherlodoc.js /tmp/html
  $ cp db.js /tmp/html
  $ firefox /tmp/html/page/index.html
  $ grep -E -o "'[\./]*db\.js" html/page/index.html
  'db.js
  $ grep -E -o "'[\./]*db\.js" html/page/Main/index.html
  '../db.js


