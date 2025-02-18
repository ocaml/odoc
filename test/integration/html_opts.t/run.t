  $ ocamlc -c -bin-annot test.mli
  $ ocamlc -c -bin-annot test2.mli
  $ printf "{0 The title}\n" > page.mld
  $ odoc compile --package test test.cmti
  $ odoc compile --package test -I . test2.cmti
  $ odoc compile --package test -I . page.mld
  $ odoc link test.odoc
  $ odoc link test2.odoc
  $ odoc link page-page.odoc
  $ odoc html-generate test.odocl -o html --indent

  $ odoc html-targets test.odocl -o html --indent
  html/test/Test/index.html

This should have the breadcrumbs in it as a nav with id 'odoc-nav'. Let's also
check it's got the expected content by looking for 'type-t'

  $ grep odoc-nav html/test/Test/index.html
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
  $ grep type-t html/test/Test/index.html
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>

Generate --as-json embeddable HTML fragment output:

  $ odoc html-generate test.odocl -o html --as-json --indent
  $ cat html/test/Test/index.html.json
  {"header":"<h1>Module <code><span>Test</span></code></h1>","type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Index","href":"../../index.html","kind":"leaf-page"},{"name":"test","href":"../index.html","kind":"page"},{"name":"Test","href":"#","kind":"module"}],"toc":[{"title":"Section 1","href":"#section-1","children":[]},{"title":"Section 2","href":"#section-2","children":[]}],"source_anchor":null,"preamble":"<p>Test</p>","content":"<h2 id=\"section-1\"><a href=\"#section-1\" class=\"anchor\"></a>Section 1</h2><div class=\"odoc-spec\">\u000A <div class=\"spec type anchored\" id=\"type-t\">\u000A  <a href=\"#type-t\" class=\"anchor\"></a>\u000A  <code><span><span class=\"keyword\">type</span> t</span></code>\u000A </div>\u000A</div><h2 id=\"section-2\"><a href=\"#section-2\" class=\"anchor\"></a>Section 2</h2><div class=\"odoc-spec\">\u000A <div class=\"spec type anchored\" id=\"type-u\">\u000A  <a href=\"#type-u\" class=\"anchor\"></a>\u000A  <code><span><span class=\"keyword\">type</span> u</span></code>\u000A </div>\u000A</div>"}

  $ odoc html-targets test.odocl -o html --as-json --indent
  html/test/Test/index.html.json

Also for pages

  $ odoc html-generate -o html --as-json page-page.odocl
  $ cat html/test/page.html.json
  {"header":"<h1 id=\"the-title\"><a href=\"#the-title\" class=\"anchor\"></a>The title</h1>","type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Index","href":"../index.html","kind":"leaf-page"},{"name":"test","href":"index.html","kind":"page"},{"name":"page","href":"#","kind":"leaf-page"}],"toc":[],"source_anchor":null,"preamble":"","content":""}


Check semantic_uris:
  $ odoc html-generate test2.odocl -o html --indent
  $ grep Test.t html/test/Test2/index.html
         <a href="../Test/index.html#type-t">Test.t</a>
  $ odoc html-generate test2.odocl -o html --semantic-uris --indent
  $ grep Test.t html/test/Test2/index.html
         <a href="../Test/#type-t">Test.t</a>
