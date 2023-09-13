A quick test to repro the issue found in #342

  $ ocamlc -bin-annot -c foo.mli

  $ odoc compile foo.cmti
  $ odoc link foo.odoc

  $ odoc html-generate --indent -o html/ foo.odocl

The table of content:

  $ cat html/Foo/index.html | grep "odoc-toc" -A 9
    <nav class="odoc-toc">
     <ul>
      <li>
       <a href="#references--and-with-text-in-title">References <code>A</code>
         and with text in title
       </a>
      </li>
      <li>
       <a href="#an-url--and-with-text-in-a-title">An url http://ocaml.org
         and with text in a title

The rendered headings

  $ cat html/Foo/index.html | grep "<h2" -A 3
     <h2 id="references--and-with-text-in-title">
      <a href="#references--and-with-text-in-title" class="anchor"></a>
      References <a href="A/index.html"><code>A</code></a> and 
      <a href="A/index.html" title="A">with text</a> in title
  --
     <h2 id="an-url--and-with-text-in-a-title">
      <a href="#an-url--and-with-text-in-a-title" class="anchor"></a>An
       url <a href="http://ocaml.org">http://ocaml.org</a> and 
      <a href="http://ocaml.org">with text</a> in a title

