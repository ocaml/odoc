A page can have source children.

  $ odoc compile -c module-a -c module-b -C lib/a/a.ml -C lib/b/b.ml root.mld

Compile the modules:

  $ ocamlc -c a.ml -bin-annot
  $ ocamlc -c b.ml -bin-annot

Now, compile the pages with the --source option. The source-name must be included in the source-children of the source-parent:

  $ odoc compile -I . --source-name lib/a/a.ml --source-parent-file page-root.odoc a.cmt
  $ odoc compile -I . --source-name lib/b/b.ml --source-parent-file page-root.odoc b.cmt
  $ odoc link -I . page-root.odoc
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc html-generate --indent -o html page-root.odocl
  $ odoc html-generate --source a.ml --indent -o html a.odocl
  $ odoc html-generate --source a.ml --indent -o html b.odocl

Source pages and source directory pages are generated:

  $ find html
  html
  html/root
  html/root/source
  html/root/source/lib
  html/root/source/lib/b
  html/root/source/lib/b/b.ml.html
  html/root/source/lib/b/index.html
  html/root/source/lib/index.html
  html/root/source/lib/a
  html/root/source/lib/a/a.ml.html
  html/root/source/lib/a/index.html
  html/root/source/index.html
  html/root/index.html
  html/A
  html/A/index.html
  html/B
  html/B/index.html

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
    <div class="odoc-content">
     <ul><li><a href="b/index.html">b</a></li>
      <li><a href="a/index.html">a</a></li>
     </ul>
    </div>
   </body>
  </html>
