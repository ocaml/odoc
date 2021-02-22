# Testing {!modules:...} lists

  $ compile external.mli main.mli

Everything should resolve:

  $ odoc_print main.odocl | jq -c '.. | .["`Modules"]? | select(.) | .[]'
  [{"`Resolved":{"`Identifier":{"`Root":[{"`RootPage":"test"},"External"]}}},"None"]
  [{"`Resolved":{"`Module":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"External"]}},"X"]}},"None"]
  [{"`Resolved":{"`Identifier":{"`Root":[{"`RootPage":"test"},"Main"]}}},"None"]
  [{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Internal"]}}},"None"]
  [{"`Resolved":{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Internal"]}},"Y"]}},"None"]

As HTML, should render as a description list.

  $ odoc html-generate --indent -o html main.odocl

  $ cat html/test/Main/index.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>Main (test.Main)</title>
    <link rel="stylesheet" href="../../odoc.css"/><meta charset="utf-8"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="../../highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body>
    <nav><a href="../index.html">Up</a> – <a href="../index.html">test</a>
      &#x00BB; Main
    </nav>
    <header><h1>Module <code>Main</code></h1>
     <ul class="modules">
      <li><a href="../External/index.html"><code>External</code></a></li>
      <li><a href="../External/X/index.html"><code>External.X</code></a></li>
      <li><a href="#"><code>Main</code></a></li>
      <li><a href="Internal/index.html"><code>Internal</code></a></li>
      <li><a href="Internal/Y/index.html"><code>Internal.Y</code></a></li>
     </ul>
    </header>
    <div class="content">
     <div>
      <div class="spec module" id="module-Internal" class="anchored">
       <a href="#module-Internal" class="anchor"></a>
       <code><span class="keyword">module</span> 
        <a href="Internal/index.html">Internal</a> : 
        <span class="keyword">sig</span> ... <span class="keyword">end</span>
        </code>
      </div><div><p>Doc for <code>Internal</code>.</p></div>
     </div>
    </div>
   </body>
  </html>
