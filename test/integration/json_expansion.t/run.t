Test the JSON output in the presence of expanded modules.

  $ ocamlc -c -bin-annot -o main__A.cmo a.ml -I .
  $ ocamlc -c -bin-annot main.ml -I .
  $ odoc compile -I . main__A.cmt
  $ odoc compile -I . main.cmt
  $ odoc link -I . main.odoc

  $ odoc html-targets -o html main.odocl
  html/Main/index.html
  html/Main/A/index.html
  html/Main/A/B/index.html
  $ odoc html-targets --as-json -o html main.odocl
  html/Main/index.html.json
  html/Main/A/index.html.json
  html/Main/A/B/index.html.json

  $ odoc html-generate --as-json -o html main.odocl

  $ cat html/Main/index.html.json
  {"type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Main","href":"#","kind":"module"}],"toc":[],"source_anchor":null,"preamble":"","content":"<div class=\"odoc-spec\"><div class=\"spec module anchored\" id=\"module-A\"><a href=\"#module-A\" class=\"anchor\"></a><code><span><span class=\"keyword\">module</span> <a href=\"A/index.html\">A</a></span><span> : <span class=\"keyword\">sig</span> ... <span class=\"keyword\">end</span></span></code></div></div>"}

  $ cat html/Main/A/index.html.json
  {"type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Main","href":"../index.html","kind":"module"},{"name":"A","href":"#","kind":"module"}],"toc":[],"source_anchor":null,"preamble":"","content":"<div class=\"odoc-spec\"><div class=\"spec module anchored\" id=\"module-B\"><a href=\"#module-B\" class=\"anchor\"></a><code><span><span class=\"keyword\">module</span> <a href=\"B/index.html\">B</a></span><span> : <span class=\"keyword\">sig</span> ... <span class=\"keyword\">end</span></span></code></div></div>"}

  $ cat html/Main/A/B/index.html.json
  {"type":"documentation","uses_katex":false,"breadcrumbs":[{"name":"Main","href":"../../index.html","kind":"module"},{"name":"A","href":"../index.html","kind":"module"},{"name":"B","href":"#","kind":"module"}],"toc":[],"source_anchor":null,"preamble":"","content":""}
