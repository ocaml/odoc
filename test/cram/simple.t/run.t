  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile --child module-main -I . page.mld
  $ odoc compile --parent page -I . main.cmt
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
  $ du -sh megaodocl
  12K	megaodocl
  $ sherlodoc index --format=js --db=db.js $(find . -name '*.odocl') 2> /dev/null

Here cat is used to remove weird permissions on executable built by dune
  $ cat ../../../jsoo/main.bc.js > sherlodoc.js
  $ du -sh sherlodoc.js
  96K	sherlodoc.js
  $ mkdir html
  $ cp sherlodoc.js html
  $ cp db.js html
  $ odoc support-files -o html
  $ for f in $(find . -name '*.odocl'); do
  >  echo $f ;
  >  cd html ;
  >  odoc html-generate --search-uri db.js --search-uri sherlodoc.js --output-dir . ../$f ;
  >  cd ..
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
  db.js
  fonts
  highlight.pack.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  page
  sherlodoc.js
  $ ls html/page
  Main
  index.html
  $ find . -name "*.html" -type f | sort
  ./html/page/Main/Modulule/index.html
  ./html/page/Main/Trucmuche/index.html
  ./html/page/Main/class-istack/index.html
  ./html/page/Main/class-type-my_class_type/index.html
  ./html/page/Main/index.html
  ./html/page/Main/module-type-Signature/index.html
  ./html/page/index.html
  $ find . -name "*.js" -type f | sort
  ./db.js
  ./html/db.js
  ./html/highlight.pack.js
  ./html/katex.min.js
  ./html/odoc_search.js
  ./html/sherlodoc.js
  ./sherlodoc.js

Indent to see results
$ cp -r html /tmp
$ firefox /tmp/html/page/index.html
  $ grep -E -o "'[\./a-zA-Z0-9_]*\.js" html/page/index.html
  '../db.js
  '../sherlodoc.js


