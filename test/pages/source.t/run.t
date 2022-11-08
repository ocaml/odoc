Files containing some values:

  $ cat a.ml
  let x = 2
  let y = x + 1
  let z = x + y
  $ cat a.mli
  val x : int
  val y : int
  val z : int

Compile the modules:

  $ ocamlc -c a.mli -bin-annot

Compile the pages:

  $ odoc compile --impl a.ml --intf a.mli a.cmti
  File "a.cmti":
  Warning: No implementation file found for the given interface
  $ for i in *.odoc; do odoc link -I . $i; done
  $ odoc html -o html a.odoc

Check the generated pages:

  $ find html -type f | sort
  html/A/index.html
  html/A/index.ml.html
  html/A/index.mli.html

  $ cat html/A/index.ml.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>A (A)</title><link rel="stylesheet" href="../odoc-src.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/></head><body class="odoc-src"><pre><code><span><span class="LET"><span>let</span></span><span> </span><span class="LIDENTx"><span>x</span></span><span> </span><span class="EQUAL"><span>=</span></span><span> </span><span class="INT"><span>2</span></span><span class="EOL"><span>
  </span></span><span class="LET"><span>let</span></span><span> </span><span class="LIDENTy"><span>y</span></span><span> </span><span class="EQUAL"><span>=</span></span><span> </span><span class="LIDENTx"><span>x</span></span><span> </span><span class="PLUS"><span>+</span></span><span> </span><span class="INT"><span>1</span></span><span class="EOL"><span>
  </span></span><span class="LET"><span>let</span></span><span> </span><span class="LIDENTz"><span>z</span></span><span> </span><span class="EQUAL"><span>=</span></span><span> </span><span class="LIDENTx"><span>x</span></span><span> </span><span class="PLUS"><span>+</span></span><span> </span><span class="LIDENTy"><span>y</span></span><span class="EOL"><span>
  </span></span></span></code></pre></body></html>

  $ cat html/A/index.mli.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml"><head><title>A (A)</title><link rel="stylesheet" href="../odoc-src.css"/><meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/><meta name="viewport" content="width=device-width,initial-scale=1.0"/></head><body class="odoc-src"><pre><code><span><span class="VAL"><span>val</span></span><span> </span><span class="LIDENTx"><span>x</span></span><span> </span><span class="COLON"><span>:</span></span><span> </span><span class="LIDENTint"><span>int</span></span><span class="EOL"><span>
  </span></span><span class="VAL"><span>val</span></span><span> </span><span class="LIDENTy"><span>y</span></span><span> </span><span class="COLON"><span>:</span></span><span> </span><span class="LIDENTint"><span>int</span></span><span class="EOL"><span>
  </span></span><span class="VAL"><span>val</span></span><span> </span><span class="LIDENTz"><span>z</span></span><span> </span><span class="COLON"><span>:</span></span><span> </span><span class="LIDENTint"><span>int</span></span><span class="EOL"><span>
  </span></span></span></code></pre></body></html>
