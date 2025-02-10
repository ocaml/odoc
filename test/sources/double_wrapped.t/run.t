This is what happens when a dune user write a toplevel module.
Similar to the lookup_def_wrapped test.

  $ ocamlc -c -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c -o main__.cmo main__.ml -bin-annot -I .
  $ ocamlc -c -open Main__ main.ml -bin-annot -I .

  $ odoc compile-impl --source-id src/a.ml -I . main__A.cmt --output-dir .
  $ odoc compile -I . main__A.cmt
  $ odoc compile-impl --source-id src/main__.ml -I . main__.cmt --output-dir .
  $ odoc compile -I . main__.cmt
  $ odoc compile-impl --source-id src/main.ml -I . main.cmt --output-dir .
  $ odoc compile -I . main.cmt

  $ odoc link -I . main.odoc
  $ odoc link -I . impl-main__A.odoc
  $ odoc link -I . impl-main.odoc
  $ odoc link -I . impl-main__.odoc
  $ odoc link -I . main__A.odoc
  $ odoc link -I . main__.odoc

  $ odoc html-generate --indent -o html main.odocl
  $ odoc html-generate-source --impl impl-main.odocl --indent -o html main.ml
  $ odoc html-generate-source --impl impl-main__A.odocl --indent -o html a.ml

Look if all the source files are generated:

  $ find html | sort
  html
  html/Main
  html/Main/A
  html/Main/A/index.html
  html/Main/index.html
  html/src
  html/src/a.ml.html
  html/src/main.ml.html

  $ cat html/Main/A/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>A (Main.A)</title><meta charset="utf-8"/>
    <link rel="stylesheet" href="../../odoc.css"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../index.html">Index</a> &#x00BB; 
     <a href="../index.html">Main</a> &#x00BB; A
    </nav>
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../src/a.ml.html" class="source_link">Source</a>
     </h1>
    </header>
    <div class="odoc-content">
     <div class="odoc-spec">
      <div class="spec value anchored" id="val-x">
       <a href="#val-x" class="anchor"></a>
       <a href="../../src/a.ml.html#val-x" class="source_link">Source</a>
       <code><span><span class="keyword">val</span> x : int</span></code>
      </div>
     </div>
    </div>
   </body>
  </html>
