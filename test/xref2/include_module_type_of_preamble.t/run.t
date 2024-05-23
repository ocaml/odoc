  $ compile foo.mli bar.ml
  $ odoc html-generate -o html --indent foo.odocl
  $ odoc html-generate -o html --indent bar.odocl

Foo contains "Preamble for O" once.

  $ cat html/test/Foo/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>Foo (test.Foo)</title><meta charset="utf-8"/>
    <link rel="stylesheet" href="../../odoc.css"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../index.html">test</a> &#x00BB; Foo
    </nav>
    <header class="odoc-preamble"><h1>Module <code><span>Foo</span></code></h1>
     <p>Preamble for Foo.</p>
    </header>
    <div class="odoc-content">
     <div class="odoc-spec">
      <div class="spec module anchored" id="module-O">
       <a href="#module-O" class="anchor"></a>
       <code>
        <span><span class="keyword">module</span> <a href="O/index.html">O</a>
        </span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div><div class="spec-doc"><p>Preamble for O.</p></div>
     </div>
     <div class="odoc-spec">
      <div class="spec value anchored" id="val-x">
       <a href="#val-x" class="anchor"></a>
       <code><span><span class="keyword">val</span> x : int</span></code>
      </div>
     </div>
    </div>
   </body>
  </html>

Bar includes Foo and should also contain "Preamble for O" once.
Bar doesn't contain "Preamble for Foo" on purpose.

  $ cat html/test/Bar/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>Bar (test.Bar)</title><meta charset="utf-8"/>
    <link rel="stylesheet" href="../../odoc.css"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../index.html">test</a> &#x00BB; Bar
    </nav>
    <header class="odoc-preamble"><h1>Module <code><span>Bar</span></code></h1>
    </header>
    <div class="odoc-content">
     <div class="odoc-include">
      <details open="open">
       <summary class="spec include">
        <code>
         <span><span class="keyword">include</span> 
          <span class="keyword">module</span> <span class="keyword">type</span>
           <span class="keyword">of</span> <span class="keyword">struct</span>
           <span class="keyword">include</span> 
          <a href="../Foo/index.html">Foo</a> <span class="keyword">end</span>
         </span>
        </code>
       </summary>
       <div class="odoc-spec">
        <div class="spec module anchored" id="module-O">
         <a href="#module-O" class="anchor"></a>
         <code><span><span class="keyword">module</span> O</span>
          <span> = <a href="../Foo/O/index.html">Foo.O</a></span>
         </code>
        </div><div class="spec-doc"><p>Preamble for O.</p></div>
       </div>
       <div class="odoc-spec">
        <div class="spec value anchored" id="val-x">
         <a href="#val-x" class="anchor"></a>
         <code><span><span class="keyword">val</span> x : int</span></code>
        </div>
       </div>
      </details>
     </div>
    </div>
   </body>
  </html>
