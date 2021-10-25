
  $ compile test.mli
  Duplicate found: (root Test).B
  File "test.mli", line 27, characters 14-20:
  Failed to resolve reference unresolvedroot(M).C Couldn't find "C"
  File "test.mli", line 27, characters 9-13:
  Reference to label 'B' is ambiguous.
  This reference will point to the first occurence of 'B'.
  Hint:
    Define labels explicitly using the syntax '{1:explicit-label Heading text}'.
  File "test.mli", line 21, characters 4-22:
  Label 'B' is ambiguous. The other occurences are:
    File "test.mli", line 21, character 4
    File "test.mli", line 3, character 4
  File "test.mli", line 18, characters 6-10:
  Reference to label 'B' is ambiguous.
  This reference will point to the first occurence of 'B'.
  Hint:
    Define labels explicitly using the syntax '{1:explicit-label Heading text}'.
  File "test.mli", line 16, characters 6-38:
  Label 'B' is ambiguous. The other occurences are:
    File "test.mli", line 16, character 6
    File "test.mli", line 21, character 4
    File "test.mli", line 3, character 4
  File "test.mli", line 10, characters 6-41:
  Label 'B' is ambiguous. The other occurences are:
    File "test.mli", line 10, character 6
    File "test.mli", line 21, character 4
    File "test.mli", line 3, character 4
  File "test.mli", line 3, characters 4-24:
  Label 'B' is ambiguous. The other occurences are:
    File "test.mli", line 21, character 4
    File "test.mli", line 3, character 4

Labels:
Some are not in order because the 'doc' field appears after the rest in the output.

  $ odoc_print test.odocl | jq -c '.. | .["`Heading"]? | select(.) | .[1]'
  {"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"B"]}
  {"`Label":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]},"D"]}
  {"`Label":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]},"B"]}
  {"`Label":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]},"C"]}
  {"`Label":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"N"]},"B"]}
  {"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"B"]}
  {"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"A"]}

References to the labels:
We expect resolved references and the heading text filled in.

  $ odoc_print test.odocl | jq -c '.. | .["`Reference"]? | select(.)'
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"N"]},"B"]}}},[{"`Word":"An"},"`Space",{"`Word":"other"},"`Space",{"`Word":"conflicting"},"`Space",{"`Word":"label"}]]
  [{"`Resolved":{"`Label":[{"`Identifier":{"`Module":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"M"]}},"B"]}},[]]
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"A"]}}},[{"`Word":"First"},"`Space",{"`Word":"label"}]]
  [{"`Resolved":{"`Identifier":{"`Label":[{"`Root":[{"Some":{"`Page":["None","test"]}},"Test"]},"B"]}}},[{"`Word":"Dupplicate"},"`Space",{"`Word":"B"}]]
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

The second occurence of 'B' in the main page should be disambiguated
(renamed 'B_2').

  $ cat html/test/Test/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>Test (test.Test)</title>
    <link rel="stylesheet" href="../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc">
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../index.html">test</a> &#x00BB; Test
    </nav>
    <header class="odoc-preamble">
     <h1>Module <code><span>Test</span></code></h1>
    </header>
    <nav class="odoc-toc">
     <ul><li><a href="#A">First label</a></li>
      <li><a href="#B">Floating label</a></li>
      <li><a href="#B_4">Dupplicate B</a></li>
     </ul>
    </nav>
    <div class="odoc-content">
     <h2 id="A"><a href="#A" class="anchor"></a>First label</h2>
     <h2 id="B"><a href="#B" class="anchor"></a>Floating label</h2>
     <div class="odoc-spec">
      <div class="spec module" id="module-M" class="anchored">
       <a href="#module-M" class="anchor"></a>
       <code><span><span class="keyword">module</span> </span>
        <span><a href="M/index.html">M</a></span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div>
     </div>
     <div class="odoc-spec">
      <div class="spec module" id="module-N" class="anchored">
       <a href="#module-N" class="anchor"></a>
       <code><span><span class="keyword">module</span> </span>
        <span><a href="N/index.html">N</a></span>
        <span> : <span class="keyword">sig</span> ... 
         <span class="keyword">end</span>
        </span>
       </code>
      </div>
     </div><h2 id="B_4"><a href="#B_4" class="anchor"></a>Dupplicate B</h2>
     <p>Define <code>B</code> again in the same scope.</p>
     <p>References to the labels:</p>
     <p><a href="#A">First label</a> <a href="#B">Dupplicate B</a> 
      <code>M</code>.C <a href="M/index.html#D"><code>D</code></a> 
      <a href="M/index.html#B"><code>B</code></a> 
      <a href="N/index.html#B"><code>B</code></a>
     </p>
    </div>
   </body>
  </html>
