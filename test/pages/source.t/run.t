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

  $ odoc compile a.cmti
  File "a.cmti":
  Warning: No implementation file found for the given interface
  $ for i in *.odoc; do odoc link -I . $i; done
  $ odoc html --impl a.ml --intf a.mli --indent -o html a.odoc

Check the generated pages:

  $ find html -type f | sort
  html/A/index.html
  html/a.ml
  html/a.mli

  $ cat html/a.ml
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>a.ml (a.ml)</title><link rel="stylesheet" href="odoc.css"/>
    <meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc"><header class="odoc-preamble"></header>
    <div class="odoc-content"><p>let x = 2</p><p>let y = x + 1</p>
     <p>let z = x + y</p><p></p>
    </div>
   </body>
  </html>

  $ cat html/a.mli
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>a.mli (a.mli)</title><link rel="stylesheet" href="odoc.css"/>
    <meta charset="utf-8"/><meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc"><header class="odoc-preamble"></header>
    <div class="odoc-content"><p>val x : int</p><p>val y : int</p>
     <p>val z : int</p><p></p>
    </div>
   </body>
  </html>
