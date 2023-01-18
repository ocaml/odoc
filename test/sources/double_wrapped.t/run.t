This is what happens when a dune user write a toplevel module.
Similar to the lookup_def_wrapped test.

  $ ocamlc -c -o main__A.cmo a.ml -bin-annot -I .
  $ ocamlc -c -o main__.cmo main__.ml -bin-annot -I .
  $ ocamlc -c -open Main__ main.ml -bin-annot -I .

  $ odoc compile --impl a.ml -I . main__A.cmt
  $ odoc compile --impl main__.ml -I . main__.cmt
  $ odoc compile --impl main.ml -I . main.cmt

  $ odoc link -I . main.odoc
  $ odoc link -I . main__A.odoc
  $ odoc link -I . main__.odoc

  $ odoc html-generate --indent -o html main.odocl
  $ odoc html-generate --hidden --indent -o html main__.odocl
  $ odoc html-generate --hidden --indent -o html main__A.odocl

Look if all the source files are generated:

  $ find html | sort
  html
  html/Main
  html/Main/A
  html/Main/A/index.html
  html/Main/Main.ml.html
  html/Main/index.html
  html/Main__
  html/Main__/Main__.ml.html
  html/Main__A
  html/Main__A/Main__A.ml.html

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
     <h1>Module <code><span>Main.A</span></code></h1>
    </header>
    <div class="odoc-content">
     <div class="odoc-spec">
      <div class="spec value anchored" id="val-x">
       <a href="#val-x" class="anchor"></a>
       <a href="../../Main__A/Main__A.ml.html#def-Main__A0" class="source_link">
        Source
       </a><code><span><span class="keyword">val</span> x : int</span></code>
      </div>
     </div>
    </div>
   </body>
  </html>

  $ cat html/Main__A/Main__A.ml.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>Source: Main__A.ml (Main__A)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/></head><body class="odoc-src"><pre class="source_container"><code class="source_line_column"><a id="L1" class="source_line" href="#L1">1</a>
  </code><code class="source_code"><span><span class="LET">let</span> <span id="x_267"><span id="def-Main__A0"><span class="LIDENT">x</span></span></span> <span class="EQUAL">=</span> <span class="INT">1</span><span class="EOL">
  </span></span></code></pre></body></html>
