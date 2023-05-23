 $ git clone git@github.com:aantron/dream.git
  $ cd dream
  cd: dream: No such file or directory
  [1]
  $ dune build @doc 2> /dev/null
  $ pwd
  $TESTCASE_ROOT
  $ cd ..
  $ find . -name '*.odocl'
  $ odoc compile-index --binary -I dream/_build/default/_doc/_odocls/playground -I dream/_build/default/_doc/_odocls/dream-pure -I dream/_build/default/_doc/_odocls/hello -o index.odoc_bin
  odoc: unknown option '--binary'.
  Usage: odoc compile-index [OPTION]…
  Try 'odoc compile-index --help' or 'odoc --help' for more information.
  [2]
  $ sherlodoc_index --format=js --odoc=index.odoc_bin  --db=db.js      
  index: option '--odoc': no 'index.odoc_bin' file or directory
  Usage: index [--db=DB] [--format=DB_FORMAT] [--odoc=ODOC_FILE] [OPTION]…
  Try 'index --help' for more information.
  [124]
  $ du -sh db.js
  du: cannot access 'db.js': No such file or directory
  [1]
  $ odoc html-generate --with-search --output-dir html dream/_build/default/_doc/_odocls/playground/page-index.odocl
  odoc: FILE.odocl argument: no
        'dream/_build/default/_doc/_odocls/playground/page-index.odocl' file or
        directory
  Usage: odoc html-generate [OPTION]… FILE.odocl
  Try 'odoc html-generate --help' or 'odoc --help' for more information.
  [2]
  $ odoc html-generate --with-search --output-dir html dream/_build/default/_doc/_odocls/dream-pure/page-index.odocl
  odoc: FILE.odocl argument: no
        'dream/_build/default/_doc/_odocls/dream-pure/page-index.odocl' file or
        directory
  Usage: odoc html-generate [OPTION]… FILE.odocl
  Try 'odoc html-generate --help' or 'odoc --help' for more information.
  [2]
  $ odoc html-generate --with-search --output-dir html dream/_build/default/_doc/_odocls/dream-pure/dream_pure.odocl
  odoc: FILE.odocl argument: no
        'dream/_build/default/_doc/_odocls/dream-pure/dream_pure.odocl' file or
        directory
  Usage: odoc html-generate [OPTION]… FILE.odocl
  Try 'odoc html-generate --help' or 'odoc --help' for more information.
  [2]
  $ odoc html-generate --with-search --output-dir html dream/_build/default/_doc/_odocls/hello/page-index.odocl
  odoc: FILE.odocl argument: no
        'dream/_build/default/_doc/_odocls/hello/page-index.odocl' file or
        directory
  Usage: odoc html-generate [OPTION]… FILE.odocl
  Try 'odoc html-generate --help' or 'odoc --help' for more information.
  [2]
  $ ls
  html
  odig
  $ ls dream/_build/default/_doc/_odocls/dream-pure
  ls: cannot access 'dream/_build/default/_doc/_odocls/dream-pure': No such file or directory
  [2]
  $ odoc support-files -o html
  $ cat db.js  ../../../bin/JSherlodoc/main.bc.js > html/index.js
  cat: db.js: No such file or directory
  cat: ../../../bin/JSherlodoc/main.bc.js: No such file or directory
  [1]
  $ du -sh html/index.js
  0	html/index.js
  $ ls html
  fonts
  highlight.pack.js
  index.js
  katex.min.css
  katex.min.js
  odoc.css
  odoc_search.js
  $ ls html/dream-pure
  ls: cannot access 'html/dream-pure': No such file or directory
  [2]
  $ cp -r html /tmp
  $ xdg-open /tmp/html/dream-pure/index.html
