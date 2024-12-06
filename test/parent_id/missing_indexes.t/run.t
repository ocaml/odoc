  $ echo "{0 Foo}" > foo.mld
  $ echo "{0 Bar}" > bar.mld
  $ echo "{0 Bli}" > bli.mld
  $ odoc compile --parent-id "" --output-dir _odoc foo.mld
  $ odoc compile --parent-id "" --output-dir _odoc bar.mld
  $ odoc compile --parent-id "baz" --output-dir _odoc bli.mld
  $ odoc link _odoc/page-foo.odoc
  $ odoc link _odoc/page-bar.odoc
  $ odoc link _odoc/baz/page-bli.odoc
  $ odoc compile-index --root _odoc
  $ odoc sidebar-generate index.odoc-index

  $ odoc html-generate --sidebar sidebar.odoc-sidebar --indent --output-dir _html _odoc/page-foo.odocl

Missing index for Baz makes it unclickable but use the ID for the name.
Root is used for the missing index in the unnamed root directory. TODO
  $ cat _html/foo.html
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
   <head><title>foo (foo)</title><meta charset="utf-8"/>
    <link rel="stylesheet" href="odoc.css"/>
    <meta name="generator" content="odoc %%VERSION%%"/>
    <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    <script src="highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
   </head>
   <body class="odoc"><nav class="odoc-nav">index &#x00BB; Foo</nav>
    <header class="odoc-preamble">
     <h1 id="foo"><a href="#foo" class="anchor"></a>Foo</h1>
    </header>
    <div class="odoc-tocs">
     <nav class="odoc-toc odoc-global-toc">
      <ul>
       <li>index
        <ul><li><a href="bar.html">Bar</a></li>
         <li>baz<ul><li><a href="baz/bli.html">Bli</a></li></ul></li>
         <li><a href="#" class="current_unit">Foo</a></li>
        </ul>
       </li>
      </ul>
     </nav>
    </div><div class="odoc-content"></div>
   </body>
  </html>
