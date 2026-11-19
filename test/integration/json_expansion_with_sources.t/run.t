Test the JSON output in the presence of expanded modules.

  $ ocamlc -c -bin-annot -o main__A.cmo a.ml -I .
  $ ocamlc -c -bin-annot main.ml -I .
  $ odoc compile-impl --source-id src/a.ml -I . main__A.cmt --output-dir .
  $ odoc compile -I . main__A.cmt
  $ odoc compile-impl --source-id src/main.ml -I . main.cmt --output-dir .
  $ odoc compile -I . main.cmt
  $ odoc link -I . impl-main__A.odoc
  $ odoc link -I . impl-main.odoc
  $ odoc link -I . main__A.odoc
  $ odoc link -I . main.odoc

  $ odoc html-targets -o html main__A.odocl
  html/Main__A/index.html
  $ odoc html-targets -o html main.odocl
  html/Main/index.html
  html/Main/A/index.html
  html/Main/A/B/index.html
  $ odoc html-targets --as-json -o html main__A.odocl
  html/Main__A/index.html.json
  $ odoc html-targets --as-json -o html main.odocl
  html/Main/index.html.json
  html/Main/A/index.html.json
  html/Main/A/B/index.html.json
  $ odoc html-targets-source --impl impl-main__A.odocl -o html a.ml
  html/src/a.ml.html
  $ odoc html-targets-source --impl impl-main.odocl -o html main.ml
  html/src/main.ml.html
  $ odoc html-targets-source --impl impl-main__A.odocl --as-json -o html a.ml
  html/src/a.ml.html.json
  $ odoc html-targets-source --impl impl-main.odocl --as-json -o html main.ml
  html/src/main.ml.html.json

  $ odoc html-generate-source --impl impl-main__A.odocl --as-json -o html a.ml
  $ odoc html-generate --as-json -o html main__A.odocl
  $ odoc html-generate-source --impl impl-main.odocl --as-json -o html main.ml
  $ odoc html-generate --as-json -o html main.odocl

  $ cat html/Main/index.html.json
  {"header":"<h1>Module <code><span>Main</span></code><a href=\"../src/main.ml.html\" class=\"source_link\">Source</a></h1>","type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Index","href":"../index.html","kind":"leaf-page"},{"name":"Main","href":"#","kind":"module"}],"toc":[],"source_anchor":"../src/main.ml.html","preamble":"","content":"<div class=\"odoc-spec\"><div class=\"spec module anchored\" id=\"module-A\"><a href=\"#module-A\" class=\"anchor\"></a><a href=\"../src/a.ml.html\" class=\"source_link\">Source</a><code><span><span class=\"keyword\">module</span> <a href=\"A/index.html\">A</a></span><span> : <span class=\"keyword\">sig</span> ... <span class=\"keyword\">end</span></span></code></div></div>"}

  $ cat html/Main/A/index.html.json
  {"header":"<h1>Module <code><span>Main.A</span></code><a href=\"../../src/a.ml.html\" class=\"source_link\">Source</a></h1>","type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Index","href":"../../index.html","kind":"leaf-page"},{"name":"Main","href":"../index.html","kind":"module"},{"name":"A","href":"#","kind":"module"}],"toc":[],"source_anchor":"../../src/a.ml.html","preamble":"","content":"<div class=\"odoc-spec\"><div class=\"spec module anchored\" id=\"module-B\"><a href=\"#module-B\" class=\"anchor\"></a><a href=\"../../src/a.ml.html#module-B\" class=\"source_link\">Source</a><code><span><span class=\"keyword\">module</span> <a href=\"B/index.html\">B</a></span><span> : <span class=\"keyword\">sig</span> ... <span class=\"keyword\">end</span></span></code></div></div>"}

  $ cat html/Main/A/B/index.html.json
  {"header":"<h1>Module <code><span>A.B</span></code><a href=\"../../../src/a.ml.html#module-B\" class=\"source_link\">Source</a></h1>","type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Index","href":"../../../index.html","kind":"leaf-page"},{"name":"Main","href":"../../index.html","kind":"module"},{"name":"A","href":"../index.html","kind":"module"},{"name":"B","href":"#","kind":"module"}],"toc":[],"source_anchor":"../../../src/a.ml.html#module-B","preamble":"","content":""}

  $ cat html/src/a.ml.html.json
  {"type":"source","breadcrumbs":[{"name":"Index","href":"../index.html","kind":"leaf-page"},{"name":"src","href":"index.html","kind":"page"},{"name":"a.ml","href":"#","kind":"source"}],"global_toc":null,"header":"<h1>Source file <code><span>a.ml</span></code></h1>","content":"<pre class=\"source_container\"><code class=\"source_line_column\"><a id=\"L1\" class=\"source_line\" href=\"#L1\">1</a>\u000A</code><code class=\"source_code\"><span><span class=\"MODULE\"><span id=\"module-B\"></span>module</span> <span class=\"UIDENT\">B</span> <span class=\"EQUAL\">=</span> <span class=\"STRUCT\">struct</span> <span class=\"END\">end</span><span class=\"EOL\">\u000A</span></span></code></pre>"}
