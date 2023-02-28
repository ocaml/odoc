This is what happens when a dune user write a toplevel module.
Similar to the lookup_def_wrapped test.

  $ odoc compile -c module-a -c src-source root.mld

  $ printf "a.ml\nmain.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c -o main__.cmo main__.ml -bin-annot -I .
  $ ocamlc -c -open Main__ main.ml -bin-annot -I .

  $ odoc compile --source-name a.ml --source-parent-file src-source.odoc -I . main__A.cmt
  $ odoc compile -I . main__.cmt
  $ odoc compile --source-name main.ml --source-parent-file src-source.odoc -I . main.cmt

  $ odoc link -I . main.odoc
  $ odoc link -I . main__A.odoc
  $ odoc link -I . main__.odoc

  $ odoc html-generate --source main.ml --indent -o html main.odocl
  $ odoc html-generate --hidden --indent -o html main__.odocl
  $ odoc html-generate --source a.ml --hidden --indent -o html main__A.odocl

Look if all the source files are generated:

  $ find html | sort
  html
  html/Main
  html/Main/A
  html/Main/A/index.html
  html/Main/index.html
  html/root
  html/root/source
  html/root/source/a.ml.html
  html/root/source/main.ml.html

  $ cat html/Main/A/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>A (Main.A)</title>
    <link rel="stylesheet" href="../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../index.html">Main</a> &#x00BB; A
    </nav>
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../root/source/a.ml.html" class="source_link">Source</a>
     </h1>
    </header>
    <div class="odoc-content">
     <div class="odoc-spec">
      <div class="spec value anchored" id="val-x">
       <a href="#val-x" class="anchor"></a>
       <a href="../../root/source/a.ml.html#def-0" class="source_link">Source
       </a><code><span><span class="keyword">val</span> x : int</span></code>
      </div>
     </div>
    </div>
   </body>
  </html>
