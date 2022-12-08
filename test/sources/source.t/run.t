Files containing some values:

  $ cat a.ml
  type t = string
  let x = 2
  let y = x + 1
  let z a = if x = 1 || true then x + y else 0
  $ cat a.mli
  type t
  val x : int
  val y : int
  val z : int -> int

Compile the modules:

  $ ocamlc -c a.mli a.ml -bin-annot

Compile the pages:

  $ odoc compile --impl a.ml --intf a.mli a.cmti
  $ odoc link -I . a.odoc
  $ odoc html-generate --indent -o html a.odocl

Check the generated pages:

  $ find html -type f | sort
  html/A/A.ml.html
  html/A/A.mli.html
  html/A/index.html

  $ cat html/A/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>A (A)</title><link rel="stylesheet" href="../odoc.css"/>
    <meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <header class="odoc-preamble"><h1>Module <code><span>A</span></code></h1>
    </header>
    <div class="odoc-content">
     <div class="odoc-spec">
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="A.ml.html#def-A0" class="source_link">Source</a>
       <code><span><span class="keyword">type</span> t</span></code>
      </div>
     </div>
     <div class="odoc-spec">
      <div class="spec value anchored" id="val-x">
       <a href="#val-x" class="anchor"></a>
       <a href="A.ml.html#def-A1" class="source_link">Source</a>
       <code><span><span class="keyword">val</span> x : int</span></code>
      </div>
     </div>
     <div class="odoc-spec">
      <div class="spec value anchored" id="val-y">
       <a href="#val-y" class="anchor"></a>
       <a href="A.ml.html#def-A2" class="source_link">Source</a>
       <code><span><span class="keyword">val</span> y : int</span></code>
      </div>
     </div>
     <div class="odoc-spec">
      <div class="spec value anchored" id="val-z">
       <a href="#val-z" class="anchor"></a>
       <a href="A.ml.html#def-A3" class="source_link">Source</a>
       <code>
        <span><span class="keyword">val</span> z : 
         <span>int <span class="arrow">&#45;&gt;</span></span> int
        </span>
       </code>
      </div>
     </div>
    </div>
   </body>
  </html>

  $ cat html/A/A.ml.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>Source: A.ml (A)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/></head><body class="odoc-src"><pre><code><span><span id="def-A0"><span class="TYPE"><span id="L1" class="source_line"></span>type</span> <span class="LIDENT">t</span> <span class="EQUAL">=</span> <span class="LIDENT">string</span></span><span class="EOL">
  </span><span class="LET"><span id="L2" class="source_line"></span>let</span> <span id="x_268"><span id="def-A1"><span class="LIDENT">x</span></span></span> <span class="EQUAL">=</span> <span class="INT">2</span><span class="EOL">
  </span><span class="LET"><span id="L3" class="source_line"></span>let</span> <span id="y_269"><span id="def-A2"><span class="LIDENT">y</span></span></span> <span class="EQUAL">=</span> <a href="#def-A1"><a href="#x_268"><span class="LIDENT">x</span></a></a> <span class="PLUS">+</span> <span class="INT">1</span><span class="EOL">
  </span><span class="LET"><span id="L4" class="source_line"></span>let</span> <span id="z_270"><span id="def-A3"><span class="LIDENT">z</span></span></span> <span id="a_272"><span class="LIDENT">a</span></span> <span class="EQUAL">=</span> <span class="IF">if</span> <a href="#def-A1"><a href="#x_268"><span class="LIDENT">x</span></a></a> <span class="EQUAL">=</span> <span class="INT">1</span> <span class="BARBAR">||</span> <span class="TRUE">true</span> <span class="THEN">then</span> <a href="#def-A1"><a href="#x_268"><span class="LIDENT">x</span></a></a> <span class="PLUS">+</span> <a href="#def-A2"><a href="#y_269"><span class="LIDENT">y</span></a></a> <span class="ELSE">else</span> <span class="INT">0</span><span class="EOL">
  </span></span></code></pre></body></html>
