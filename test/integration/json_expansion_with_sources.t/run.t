Test the JSON output in the presence of expanded modules.

  $ odoc compile --child module-a --child src-source root.mld

  $ printf "a.ml\nmain.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map


  $ ocamlc -c -bin-annot -o main__A.cmo a.ml -I .
  $ ocamlc -c -bin-annot main.ml -I .
  $ odoc compile --source-name a.ml --source-parent-file src-source.odoc -I . main__A.cmt
  $ odoc compile --source-name main.ml --source-parent-file src-source.odoc -I . main.cmt
  $ odoc link -I . main__A.odoc
  $ odoc link -I . main.odoc

  $ odoc html-targets --source a.ml -o html main__A.odocl
  html/Main__A/index.html
  html/root/source/a.ml.html
  $ odoc html-targets --source main.ml -o html main.odocl
  html/Main/index.html
  html/Main/A/index.html
  html/Main/A/B/index.html
  html/root/source/main.ml.html
  $ odoc html-targets --source a.ml --as-json -o html main__A.odocl
  html/Main__A/index.html.json
  html/root/source/a.ml.html.json
  $ odoc html-targets --source main.ml --as-json -o html main.odocl
  html/Main/index.html.json
  html/Main/A/index.html.json
  html/Main/A/B/index.html.json
  html/root/source/main.ml.html.json

  $ odoc html-generate --source a.ml --as-json -o html main__A.odocl
  $ odoc html-generate --source main.ml --as-json -o html main.odocl

  $ cat html/Main/index.html.json
  {"type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Main","href":"#","kind":"module"}],"toc":[],"source_anchor":"../root/source/main.ml.html","preamble":"","content":"<div class=\"odoc-spec\"><div class=\"spec module anchored\" id=\"module-A\"><a href=\"#module-A\" class=\"anchor\"></a><a href=\"../root/source/a.ml.html\" class=\"source_link\">Source</a><code><span><span class=\"keyword\">module</span> <a href=\"A/index.html\">A</a></span><span> : <span class=\"keyword\">sig</span> ... <span class=\"keyword\">end</span></span></code></div></div>"}

  $ cat html/Main/A/index.html.json
  {"type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Main","href":"../index.html","kind":"module"},{"name":"A","href":"#","kind":"module"}],"toc":[],"source_anchor":"../../root/source/a.ml.html","preamble":"","content":"<div class=\"odoc-spec\"><div class=\"spec module anchored\" id=\"module-B\"><a href=\"#module-B\" class=\"anchor\"></a><a href=\"../../root/source/a.ml.html#module-B\" class=\"source_link\">Source</a><code><span><span class=\"keyword\">module</span> <a href=\"B/index.html\">B</a></span><span> : <span class=\"keyword\">sig</span> ... <span class=\"keyword\">end</span></span></code></div></div>"}

  $ cat html/Main/A/B/index.html.json
  {"type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Main","href":"../../index.html","kind":"module"},{"name":"A","href":"../index.html","kind":"module"},{"name":"B","href":"#","kind":"module"}],"toc":[],"source_anchor":"../../../root/source/a.ml.html#module-B","preamble":"","content":""}

  $ cat html/root/source/a.ml.html.json
  {"type":"source","breadcrumbs":[{"name":"root","href":"../index.html","kind":"page"},{"name":"source","href":"index.html","kind":"page"},{"name":"a.ml","href":"#","kind":"source"}],"content":"<pre class=\"source_container\"><code class=\"source_line_column\"><a id=\"L1\" class=\"source_line\" href=\"#L1\">1</a>\u000A</code><code class=\"source_code\"><span><span id=\"module-B\"><span class=\"MODULE\">module</span> <span class=\"UIDENT\">B</span> <span class=\"EQUAL\">=</span> <span class=\"STRUCT\">struct</span> <span class=\"END\">end</span></span><span class=\"EOL\">\u000A</span></span></code></pre>"}
