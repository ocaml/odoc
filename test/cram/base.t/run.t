wwwwwwwwwwwwwww


  $ for f in $(find . -name '*.odocl'); do
  >  odoc html-generate --with-search --output-dir html $f 2> /dev/null
  > done
  $ odoc support-files -o html
  $ cat db.js  ../../../bin/JSherlodoc/main.bc.js > html/index.js
  $ du -sh html/index.js
  13M	html/index.js
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
