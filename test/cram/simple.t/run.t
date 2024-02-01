  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile --child module-main -I . page.mld
  $ odoc compile --parent page -I . main.cmt
  $ odoc link -I . main.odoc
  $ odoc link -I . page-page.odoc
  $ cat $(find . -name '*.odocl') > megaodocl
$ du -sh megaodocl
12K	megaodocl
  $ mkdir html
  $ sherlodoc index --format=js --db=html/db.js $(find . -name '*.odocl') 2> /dev/null
  $ sherlodoc js html/sherlodoc.js
  $ odoc support-files -o html
  $ for f in $(find . -name '*.odocl' | sort); do
  >  echo $f ;
  >  cd html ;
  >  odoc html-generate --search-uri db.js --search-uri sherlodoc.js --output-dir . ../$f ;
  >  cd ..
  > done | sort
  ./main.odocl
  ./page-page.odocl
  $ ls | sort
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
  $ ls html | sort
  db.js
  fonts
  highlight.pack.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  page
  sherlodoc.js
  $ ls html/page | sort
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
  ./html/db.js
  ./html/highlight.pack.js
  ./html/katex.min.js
  ./html/odoc_search.js
  ./html/sherlodoc.js

Indent to see results
$ cp -r html /tmp
$ firefox /tmp/html/page/index.html
  $ grep -E -o "'[\./a-zA-Z0-9_]*\.js" html/page/index.html
  '../db.js
  '../sherlodoc.js


