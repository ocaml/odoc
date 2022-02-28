  $ ocamlc -c -bin-annot test.mli
  $ ocamlc -c -bin-annot test2.mli
  $ odoc compile --package test test.cmti
  $ odoc compile --package test -I . test2.cmti
  $ odoc link test.odoc
  $ odoc link test2.odoc
  $ odoc html-generate test.odocl -o html --indent

This should have the breadcrumbs in it as a nav with id 'odoc-nav'. Let's also
check it's got the expected content by looking for 'type-t'

  $ grep odoc-nav html/test/Test/index.html
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
  $ grep type-t html/test/Test/index.html
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>

Generate again, this time omitting the breadcrumbs:

  $ odoc html-generate test.odocl -o html --omit-breadcrumbs --indent
  $ grep odoc-nav html/test/Test/index.html
  [1]
  $ grep type-t html/test/Test/index.html
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>

Check semantic_uris:
  $ odoc html-generate test2.odocl -o html --indent
  $ grep Test.t html/test/Test2/index.html
         <a href="../Test/index.html#type-t">Test.t</a>
  $ odoc html-generate test2.odocl -o html --semantic-uris --indent
  $ grep Test.t html/test/Test2/index.html
         <a href="../Test/#type-t">Test.t</a>

Check omission of toc:
  $ odoc html-generate test.odocl -o html --indent
  $ grep odoc-toc html/test/Test/index.html
    <nav class="odoc-toc">
  $ odoc html-generate test.odocl -o html --indent --omit-toc
  $ grep odoc-toc html/test/Test/index.html
  [1]

Check content-only output:
  $ odoc html-generate test.odocl -o html --indent --content-only
  $ head -n 1 html/test/Test/index.html
  <div class="odoc">
 

