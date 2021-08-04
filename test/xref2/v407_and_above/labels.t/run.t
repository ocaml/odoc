
  $ compile test.mli
  File "test.mli", line 23, characters 14-20:
  Failed to resolve reference unresolvedroot(M).C

Labels:
Some are not in order because the 'doc' field appears after the rest in the output.

  $ odoc_print test.odocl | jq -c '.. | .["`Heading"]? | select(.) | .[1]["`Label"]'
  [{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"B"]
  [{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]},"D"]
  [{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]},"B"]
  [{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]},"C"]
  [{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"N"]},"B"]
  [{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"A"]

References to the labels:
We expect resolved references and the heading text filled in.

  $ odoc_print test.odocl | jq -c '.. | .["`Reference"]? | select(.)'
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"N"]},"B"]}}},[{"`Word":"An"},"`Space",{"`Word":"other"},"`Space",{"`Word":"conflicting"},"`Space",{"`Word":"label"}]]
  [{"`Resolved":{"`Label":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]}},"B"]}},[]]
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"A"]}}},[{"`Word":"First"},"`Space",{"`Word":"label"}]]
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"B"]}}},[{"`Word":"Floating"},"`Space",{"`Word":"label"}]]
  [{"`Dot":[{"`Root":["M","`TUnknown"]},"C"]},[]]
  [{"`Resolved":{"`Label":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]}},"D"]}},[]]
  [{"`Resolved":{"`Label":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]}},"B"]}},[]]
  [{"`Resolved":{"`Label":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"N"]}},"B"]}},[]]

  $ odoc html-generate --indent -o html test.odocl

There are two references in N, one should point to a local label and the other to M.

  $ cat html/test/Test/N/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>N (test.Test.N)</title>
    <link rel="stylesheet" href="../../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../index.html">test</a> &#x00BB; 
     <a href="../index.html">Test</a> &#x00BB; N
    </nav>
    <header class="odoc-preamble">
     <h1>Module <code><span>Test.N</span></code></h1>
    </header>
    <nav class="odoc-toc">
     <ul><li><a href="#B">An other conflicting label</a></li></ul>
    </nav>
    <div class="odoc-content">
     <h2 id="B"><a href="#B" class="anchor"></a>An other conflicting label</h2>
     <p><a href="#B">An other conflicting label</a> 
      <a href="../M/index.html#B"><code>B</code></a>
     </p>
    </div>
   </body>
  </html>
