A quick test to repro the issue found in #941

  $ ocamlc -bin-annot -c foo.mli

  $ odoc compile foo.cmti
  $ odoc compile page.mld
  $ odoc link -I . foo.odoc
  $ odoc link -I . page-page.odoc

  $ odoc html-generate --indent -o html/ foo.odocl
  $ odoc html-generate --indent -o html/ page-page.odocl

The rendered html

  $ cat html/Foo/index.html | grep "splice_me" -A 3
    <nav class="odoc-toc"><ul><li><a href="#splice_me">Splice me</a></li></ul>
    </nav>
    <div class="odoc-content">
     <h3 id="splice_me"><a href="#splice_me" class="anchor"></a>Splice me</h3>
     <p>Should output only the heading's text: 
      <a href="#splice_me" title="splice_me">Splice me</a> 
      <a href="#splice_me"><code>splice_me</code></a> 
      <a href="../page.html#splice_me"><code>splice_me</code></a>
     </p>
    </div>
   </body>
