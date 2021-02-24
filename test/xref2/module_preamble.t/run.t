  $ ocamlc -bin-annot -o a__b.cmi -c b.mli
  $ ocamlc -bin-annot -o a__b.cmo -c b.ml
  $ ocamlc -bin-annot -o a.cmi -c a.mli
  $ ocamlc -bin-annot -o a.cmo -c a.ml
  $ ocamlc -bin-annot -a -o a.cma a.cmo a__b.cmo

  $ odoc compile --pkg test -o a__b.odoc -I . a__b.cmti
  $ odoc compile --pkg test -o a.odoc -I . a.cmti

  $ odoc link -I . a__b.odoc
  $ odoc link -I . a.odoc

  $ odoc html-generate --indent -o html a.odocl
  $ odoc html-generate --indent -o html a__b.odocl

  $ odoc html-targets -o html a.odocl
  html/test/A/index.html
  html/test/A/B/index.html
  $ odoc html-targets -o html a__b.odocl
  html/test/A__b/index.html

  $ cat html/test/A/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>A (test.A)</title>
    <link rel="stylesheet" href="../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../index.html">test</a> &#x00BB; A
    </nav>
    <header class="odoc-preamble"><h1>Module <code><span>A</span></code></h1>
     <p>Module A.</p>
    </header>
    <div class="odoc-content">
     <div class="odoc-spec">
      <div class="spec module" id="module-B" class="anchored">
       <a href="#module-B" class="anchor"></a>
       <code><span><span class="keyword">module</span> </span>
        <span><a href="B/index.html">B</a></span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div>
     </div>
    </div>
   </body>
  </html>

`A/B` should have its documentation in the preamble:

  $ cat html/test/A/B/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>B (test.A.B)</title>
    <link rel="stylesheet" href="../../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../index.html">test</a> &#x00BB; <a href="../index.html">A</a>
      &#x00BB; B
    </nav>
    <header class="odoc-preamble"><h1>Module <code><span>A.B</span></code></h1>
    </header>
    <div class="odoc-content"><p>Module B.</p><p>Some documentation.</p>
     <div class="odoc-spec">
      <div class="spec type" id="type-t" class="anchored">
       <a href="#type-t" class="anchor"></a>
       <code><span><span class="keyword">type</span> t</span></code>
      </div>
     </div>
    </div>
   </body>
  </html>

`A__b` shouldn't render:

  $ cat html/test/A__b/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>A__b (test.A__b)</title>
    <link rel="stylesheet" href="../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../index.html">test</a> &#x00BB; A__b
    </nav>
    <header class="odoc-preamble">
     <h1>Module <code><span>A__b</span></code></h1><p>Module B.</p>
     <p>Some documentation.</p>
    </header><div class="odoc-content"></div>
   </body>
  </html>
