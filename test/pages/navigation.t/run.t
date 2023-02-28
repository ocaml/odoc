The TOC references parent and siblings pages and direct children. The hierarchy
will look like this:

| page foo
|   page bar
|   module A
|     Section X
|       module M
|     Section Y
|       module N
|   module B

Compilation is simple: page 'foo' is the parent of everything else.

  $ odoc compile -I . -c leafpage-bar -c A -c B foo.mld
  $ odoc compile -I . --parent page-foo bar.mld
  $ ocamlc -bin-annot a.ml
  $ odoc compile -I . --parent page-foo a.cmt
  $ ocamlc -bin-annot b.ml
  $ odoc compile -I . --parent page-foo b.cmt

Link and generate:

  $ odoc link -I . page-foo.odoc
  $ odoc link -I . page-bar.odoc
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc html-generate --indent -o html page-foo.odocl
  $ odoc html-generate --indent -o html page-bar.odocl
  $ odoc html-generate --indent -o html a.odocl
  $ odoc html-generate --indent -o html b.odocl

The TOC for module 'A' should look like the hierarchy drawn at the beginning:
TODO: Items are not nested inside sections

  $ sed -n '/<nav/,/<\/nav>/p' html/foo/A/index.html
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../index.html">foo</a> &#x00BB; A
    </nav>
    <nav class="odoc-toc">
     <ul><li><a href="">bar</a></li>
      <li><a href="">A</a>
       <ul><li><a href="section-x">section-x</a></li>
        <li><a href="type-t">t</a></li><li><a href="module-M">M</a></li>
        <li><a href="section-y">section-y</a></li>
        <li><a href="module-N">N</a></li>
       </ul>
      </li><li><a href="">B</a></li>
     </ul>
    </nav>

The TOC for module 'A.M' should mention its sibling 'A.N':
TODO: Sub modules TOC don't link to siblings and parents

  $ sed -n '/<nav/,/<\/nav>/p' html/foo/A/M/index.html
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../index.html">foo</a> &#x00BB; <a href="../index.html">A</a>
      &#x00BB; M
    </nav>
    <nav class="odoc-toc">
     <ul><li><a href="M">M</a><ul><li><a href="type-t">t</a></li></ul></li>
     </ul>
    </nav>

The TOC for page 'bar' should mention 'A' and 'B':
TODO: Pages TOC don't link to siblings and parents

  $ sed -n '/<nav/,/<\/nav>/p' html/foo/bar.html
    <nav class="odoc-nav"><a href="index.html">Up</a> – 
     <a href="index.html">foo</a> &#x00BB; bar
    </nav>
    <nav class="odoc-toc"><ul><li><a href="bar">bar</a></li></ul></nav>
    <div class="odoc-content"></div>
   </body>
  </html>
