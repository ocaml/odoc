A page can have source children.

  $ odoc compile -c module-a -c module-b -c src-source root.mld

  $ printf "lib/main.ml\nlib/b/b.ml\nlib/a/a.ml\n" > source.map
  $ odoc source-tree -I . --parent page-root source.map

Compile the modules:

  $ ocamlc -c lib/a/a.ml -bin-annot
  $ ocamlc -c lib/b/b.ml -bin-annot
  $ ocamlc -c lib/main.ml -bin-annot

Now, compile the pages with the --source option. The source-name must be included in the source-children of the source-parent:

  $ odoc compile -I . --source-name lib/a/a.ml --source-parent-file src-source.odoc lib/a/a.cmt
  $ odoc compile -I . --source-name lib/b/b.ml --source-parent-file src-source.odoc lib/b/b.cmt
  $ odoc compile -I . --source-name lib/main.ml --source-parent-file src-source.odoc lib/main.cmt
  $ odoc link -I . -I lib/a -I lib/b -I lib page-root.odoc
  $ odoc link -I . lib/a/a.odoc
  $ odoc link -I . lib/b/b.odoc
  $ odoc link -I . lib/main.odoc
  $ odoc link -I . src-source.odoc
  $ odoc html-generate --indent -o html page-root.odocl
  $ odoc html-generate --indent -o html src-source.odocl
  $ odoc html-generate --source-root . --indent -o html lib/a/a.odocl
  $ odoc html-generate --source-root . --indent -o html lib/b/b.odocl
  $ odoc html-generate --source-root . --indent -o html lib/main.odocl

Source pages and source directory pages are generated:

  $ find html | sort
  html
  html/A
  html/A/index.html
  html/B
  html/B/index.html
  html/Main
  html/Main/index.html
  html/root
  html/root/index.html
  html/root/source
  html/root/source/index.html
  html/root/source/lib
  html/root/source/lib/a
  html/root/source/lib/a/a.ml.html
  html/root/source/lib/a/index.html
  html/root/source/lib/b
  html/root/source/lib/b/b.ml.html
  html/root/source/lib/b/index.html
  html/root/source/lib/index.html
  html/root/source/lib/main.ml.html

A directory simply list its children:

  $ cat html/root/source/lib/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>lib (root.source.lib)</title>
    <link rel="stylesheet" href="../../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../index.html">root</a> &#x00BB; 
     <a href="../index.html">source</a> &#x00BB; lib
    </nav><header class="odoc-preamble"></header>
    <div class="odoc-content"><h1>./lib/</h1>
     <ul class="odoc-folder-list">
      <li><span class="odoc-directory"><a href="a/index.html">a</a></span></li>
      <li><span class="odoc-directory"><a href="b/index.html">b</a></span></li>
      <li><span class="odoc-file"><a href="main.ml.html">main.ml</a></span>
      </li>
     </ul>
    </div>
   </body>
  </html>
