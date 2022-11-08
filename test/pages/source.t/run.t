Files containing some values:

  $ cat a.ml
  let x = 2
  let y = x + 1
  let z a = if x = 1 || true then x + y else 0
  $ cat a.mli
  val x : int
  val y : int
  val z : int -> int

Compile the modules:

  $ ocamlc -c a.mli -bin-annot

Compile the pages:

  $ odoc compile --impl a.ml --intf a.mli a.cmti
  File "a.cmti":
  Warning: No implementation file found for the given interface
  $ for i in *.odoc; do odoc link -I . $i; done
  $ odoc html --indent -o html a.odoc

Check the generated pages:

  $ find html -type f | sort
  html/A/index.html
  html/A/index.ml.html
  html/A/index.mli.html

  $ cat html/A/index.ml.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>Source: A (A)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/></head><body class="odoc-src"><pre><code><span><span class="LET">let</span> <span class="LIDENT">x</span> <span class="EQUAL">=</span> <span class="INT">2</span><span class="EOL">
  </span><span class="LET">let</span> <span class="LIDENT">y</span> <span class="EQUAL">=</span> <span class="LIDENT">x</span> <span class="PLUS">+</span> <span class="INT">1</span><span class="EOL">
  </span><span class="LET">let</span> <span class="LIDENT">z</span> <span class="LIDENT">a</span> <span class="EQUAL">=</span> <span class="IF">if</span> <span class="LIDENT">x</span> <span class="EQUAL">=</span> <span class="INT">1</span> <span class="BARBAR">||</span> <span class="TRUE">true</span> <span class="THEN">then</span> <span class="LIDENT">x</span> <span class="PLUS">+</span> <span class="LIDENT">y</span> <span class="ELSE">else</span> <span class="INT">0</span><span class="EOL">
  </span></span></code></pre></body></html>

  $ cat html/A/index.mli.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>Source: A (A)</title><link rel="stylesheet" href="../odoc.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/></head><body class="odoc-src"><pre><code><span><span class="VAL">val</span> <span class="LIDENT">x</span> <span class="COLON">:</span> <span class="LIDENT">int</span><span class="EOL">
  </span><span class="VAL">val</span> <span class="LIDENT">y</span> <span class="COLON">:</span> <span class="LIDENT">int</span><span class="EOL">
  </span><span class="VAL">val</span> <span class="LIDENT">z</span> <span class="COLON">:</span> <span class="LIDENT">int</span> <span class="MINUSGREATER">-&gt;</span> <span class="LIDENT">int</span><span class="EOL">
  </span></span></code></pre></body></html>
