A quick test to repro the issue found in #941

  $ ocamlc -bin-annot -c foo.mli

  $ odoc compile foo.cmti
  $ odoc link foo.odoc

  $ odoc html-generate --indent -o html/ foo.odocl

The rendered html

  $ cat html/Foo/index.html | grep "<h3" -A 3
     <h3 id="splice_me"><a href="#splice_me" class="anchor"></a>Splice me</h3>
     <p>This is correct <a href="#splice_me">Splice me</a>.</p>
     <p>This is incorrect <a href="#splice_me"><code>splice_me</code></a>.</p>
    </div>
