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

  $ ocamlc -c a.mli -bin-annot
  $ ocamlc -c a.ml -bin-annot

Compile the pages:

  $ odoc compile --impl a.ml --intf a.mli a.cmti
  $ for i in *.odoc; do odoc link -I . $i; done
  $ odoc html -o html a.odoc

Check the generated pages:

  $ find html -type f | sort
  html/A/A.ml.html
  html/A/A.mli.html
  html/A/index.html

  $ cat html/A/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>A (A)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/><script src="../highlight.pack.js"></script><script>hljs.initHighlightingOnLoad();</script></head><body class="odoc"><header class="odoc-preamble"><h1>Module <code><span>A</span></code></h1></header><div class="odoc-content"><div class="odoc-spec"><div class="spec type anchored" id="type-t"><a href="#type-t" class="anchor"></a><a href="A.ml.html#L1" class="source_link">Link to source</a><code><span><span class="keyword">type</span> t</span></code></div></div><div class="odoc-spec"><div class="spec value anchored" id="val-x"><a href="#val-x" class="anchor"></a><a href="A.ml.html#L2" class="source_link">Link to source</a><code><span><span class="keyword">val</span> x : int</span></code></div></div><div class="odoc-spec"><div class="spec value anchored" id="val-y"><a href="#val-y" class="anchor"></a><a href="A.ml.html#L3" class="source_link">Link to source</a><code><span><span class="keyword">val</span> y : int</span></code></div></div><div class="odoc-spec"><div class="spec value anchored" id="val-z"><a href="#val-z" class="anchor"></a><a href="A.ml.html#L4" class="source_link">Link to source</a><code><span><span class="keyword">val</span> z : <span>int <span class="arrow">&#45;&gt;</span></span> int</span></code></div></div></div></body></html>

  $ cat html/A/A.ml.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>Source: A.ml (A)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/></head><body class="odoc-src"><pre><code><span><span id="L1"><span class="TYPE">type</span> <span class="LIDENT">t</span> <span class="EQUAL">=</span> <span class="LIDENT">string</span></span><span class="EOL">
  </span><span id="L2"><span class="LET">let</span> <span class="LIDENT">x</span> <span class="EQUAL">=</span> <span class="INT">2</span></span><span class="EOL">
  </span><span id="L3"><span class="LET">let</span> <span class="LIDENT">y</span> <span class="EQUAL">=</span> <span class="LIDENT">x</span> <span class="PLUS">+</span> <span class="INT">1</span></span><span class="EOL">
  </span><span id="L4"><span class="LET">let</span> <span class="LIDENT">z</span> <span class="LIDENT">a</span> <span class="EQUAL">=</span> <span class="IF">if</span> <span class="LIDENT">x</span> <span class="EQUAL">=</span> <span class="INT">1</span> <span class="BARBAR">||</span> <span class="TRUE">true</span> <span class="THEN">then</span> <span class="LIDENT">x</span> <span class="PLUS">+</span> <span class="LIDENT">y</span> <span class="ELSE">else</span> <span class="INT">0</span></span><span class="EOL">
  </span></span></code></pre></body></html>

  $ cat html/A/A.mli.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>Source: A.mli (A)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/></head><body class="odoc-src"><pre><code><span><span id="L1"><span class="TYPE">type</span> <span class="LIDENT">t</span></span><span class="EOL">
  </span><span id="L2"><span class="VAL">val</span> <span class="LIDENT">x</span> <span class="COLON">:</span> <span class="LIDENT">int</span></span><span class="EOL">
  </span><span id="L3"><span class="VAL">val</span> <span class="LIDENT">y</span> <span class="COLON">:</span> <span class="LIDENT">int</span></span><span class="EOL">
  </span><span id="L4"><span class="VAL">val</span> <span class="LIDENT">z</span> <span class="COLON">:</span> <span class="LIDENT">int</span> <span class="MINUSGREATER">-&gt;</span> <span class="LIDENT">int</span></span><span class="EOL">
  </span></span></code></pre></body></html>
