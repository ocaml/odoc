# Testing {!modules:...} lists

  $ compile external.mli main.mli

Everything should resolve:

  $ odoc_print main.odocl | jq -c '.. | .["`Modules"]? | select(.) | .[]'
  [{"`Resolved":{"`Identifier":{"`Root":[{"`RootPage":"test"},"External"]}}},{"Some":[{"`Word":"Doc"},"`Space",{"`Word":"for"},"`Space",{"`Code_span":"External"},{"`Word":"."}]}]
  [{"`Resolved":{"`Module":[{"`Identifier":{"`Root":[{"`RootPage":"test"},"External"]}},"X"]}},{"Some":[{"`Word":"Doc"},"`Space",{"`Word":"for"},"`Space",{"`Code_span":"X"},{"`Word":"."}]}]
  [{"`Resolved":{"`Identifier":{"`Root":[{"`RootPage":"test"},"Main"]}}},"None"]
  [{"`Resolved":{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Internal"]}}},{"Some":[{"`Word":"Doc"},"`Space",{"`Word":"for"},"`Space",{"`Code_span":"Internal"},{"`Word":"."}]}]
  [{"`Resolved":{"`Module":[{"`Identifier":{"`Module":[{"`Root":[{"`RootPage":"test"},"Main"]},"Internal"]}},"Y"]}},{"Some":[{"`Word":"Doc"},"`Space",{"`Word":"for"},"`Space",{"`Word":"Internal."},{"`Code_span":"X"},{"`Word":"."},"`Space",{"`Word":"An"},"`Space",{"`Word":"other"},"`Space",{"`Word":"sentence."}]}]

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
     <dl class="modules">
      <dt><a href="../External/index.html"><code>External</code></a></dt>
      <dd><p class="synopsis">Doc for <code>External</code>.</p></dd>
      <dt><a href="../External/X/index.html"><code>External.X</code></a></dt>
      <dd><p class="synopsis">Doc for <code>X</code>.</p></dd>
      <dt><a href="#"><code>Main</code></a></dt><dd></dd>
      <dt><a href="Internal/index.html"><code>Internal</code></a></dt>
      <dd><p class="synopsis">Doc for <code>Internal</code>.</p></dd>
      <dt><a href="Internal/Y/index.html"><code>Internal.Y</code></a></dt>
      <dd>
       <p class="synopsis">Doc for Internal.<code>X</code>. An other sentence.
       </p>
      </dd>
     </dl>
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
