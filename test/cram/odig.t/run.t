  $ git clone git@github.com:aantron/dream.git
  Cloning into 'dream'...
  $ cd dream
  $ dune build @doc 2> /dev/null
  [1]
  $ pwd
  $TESTCASE_ROOT/dream
  $ cd ..
  $ find . -name '*.odocl'
  ./dream/_build/default/_doc/_odocls/playground/page-index.odocl
  ./dream/_build/default/_doc/_odocls/dream-pure/page-index.odocl
  ./dream/_build/default/_doc/_odocls/dream-pure/dream_pure.odocl
  ./dream/_build/default/_doc/_odocls/hello/page-index.odocl
  $ odoc compile-index --binary -I dream/_build/default/_doc/_odocls/playground -I dream/_build/default/_doc/_odocls/dream-pure -I dream/_build/default/_doc/_odocls/hello -o index.odoc_bin
  $ sherlodoc_index --format=js --odoc=index.odoc_bin  --db=db.js      
  $ du -sh db.js
  196K	db.js
  $ odoc html-generate --with-search --output-dir html dream/_build/default/_doc/_odocls/playground/page-index.odocl
  $ odoc html-generate --with-search --output-dir html dream/_build/default/_doc/_odocls/dream-pure/page-index.odocl
  $ odoc html-generate --with-search --output-dir html dream/_build/default/_doc/_odocls/dream-pure/dream_pure.odocl
  $ odoc html-generate --with-search --output-dir html dream/_build/default/_doc/_odocls/hello/page-index.odocl
  $ ls
  db.js
  dream
  html
  index.odoc_bin
  $ ls dream/_build/default/_doc/_odocls/dream-pure
  dream_pure.odocl
  page-index.odocl
  $ odoc support-files -o html
  $ cat db.js  ../../../bin/JSherlodoc/main.bc.js > html/index.js
  $ du -sh html/index.js
  4.0M	html/index.js
  $ ls html
  dream-pure
  fonts
  hello
  highlight.pack.js
  index.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  playground
  $ ls html/dream-pure
  Dream_pure
  index.html
  $ cp -r html /tmp
  $ xdg-open /tmp/html/dream-pure/index.html
