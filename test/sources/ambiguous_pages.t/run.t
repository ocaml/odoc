Duplicated page names are common when generating the source hierarchy. For
example, two sub-directories can have the same name (eg. `bin/foo` and
`lib/foo`) and sub-directories can have the same name as a library or package.

Drivers must be careful to order the search path to avoid incorrect links.

The driver generates the pages for the package, libraries and source
directories under the `doc_` directory:

  $ find doc_ | sort
  doc_
  doc_/foo
  doc_/foo.mld
  doc_/foo/foo.mld
  doc_/src_
  doc_/src_.mld
  doc_/src_/lib
  doc_/src_/lib.mld
  doc_/src_/lib/foo.mld

The package named 'foo' contains a library named 'foo'.

  $ odoc compile -c page-foo -c page-src_ doc_/foo.mld
  $ odoc compile --parent page-foo -c module-Foo -I doc_ doc_/foo/foo.mld

Build the source code hierarchy:

  $ odoc compile --parent page-foo -c page-lib -I doc_ doc_/src_.mld
  $ odoc compile --parent page-src_ -c page-foo -I doc_ doc_/src_/lib.mld

TODO: The directories at the leaf don't have proper children.
They need to be passed a dummy children in order not to be a leaf page and to
be passed as the source-parent.

  $ odoc compile --parent page-lib -c page-dummy_child -I doc_/src_ doc_/src_/lib/foo.mld

Build the documentation for the modules:

  $ ocamlc -bin-annot -c lib/foo/foo.ml
  $ odoc compile --parent page-foo -I doc_/foo --source-parent-file doc_/src_/lib/page-foo.odoc --source-name foo.ml lib/foo/foo.cmt

Link everything and generate the html:

  $ odoc link -I doc_/foo -I doc_ doc_/page-foo.odoc
  $ odoc link -I lib/foo doc_/foo/page-foo.odoc
  $ odoc link -I doc_/src_ doc_/page-src_.odoc
  $ odoc link -I doc_/src_/lib doc_/src_/page-lib.odoc
  $ odoc link doc_/src_/lib/page-foo.odoc
  File "doc_/src_/lib/page-foo.odoc":
  Warning: Failed to resolve child reference unresolvedroot(dummy_child)
  $ odoc link lib/foo/foo.odoc

  $ odoc html-generate --indent -o html doc_/page-foo.odocl
  $ odoc html-generate --indent -o html doc_/foo/page-foo.odocl
  $ odoc html-generate --indent -o html doc_/page-src_.odocl
  $ odoc html-generate --indent -o html doc_/src_/page-lib.odocl
  $ odoc html-generate --indent -o html doc_/src_/lib/page-foo.odocl
  $ odoc html-generate --indent -o html --source lib/foo/foo.ml lib/foo/foo.odocl

See whether the links are right:

  $ grep href html/foo/src_/lib/foo/index.html
    <link rel="stylesheet" href="../../../../odoc.css"/><meta charset="utf-8"/>
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../../index.html">foo</a> &#x00BB; 
     <a href="../../index.html">src_</a> &#x00BB; 
     <a href="../index.html">lib</a> &#x00BB; foo
     <h1 id="lib/foo/"><a href="#lib/foo/" class="anchor"></a>lib/foo/</h1>
     <ul><li><a href="foo.ml.html">foo.ml</a></li></ul>

  $ grep href html/foo/src_/lib/index.html
    <link rel="stylesheet" href="../../../odoc.css"/><meta charset="utf-8"/>
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../../index.html">foo</a> &#x00BB; 
     <a href="../index.html">src_</a> &#x00BB; lib
     <h1 id="lib/"><a href="#lib/" class="anchor"></a>lib/</h1>
     <ul><li><a href="foo/index.html"><code>foo</code></a></li></ul>

  $ grep href html/foo/src_/index.html
    <link rel="stylesheet" href="../../odoc.css"/><meta charset="utf-8"/>
    <nav class="odoc-nav"><a href="../index.html">Up</a> – 
     <a href="../index.html">foo</a> &#x00BB; src_
     <h1 id="./"><a href="#./" class="anchor"></a>./</h1>
     <ul><li><a href="lib/index.html"><code>lib</code></a></li></ul>

  $ grep -C 1 source_link html/foo/foo/Foo/index.html
     <h1>Module <code><span>Foo</span></code>
      <a href="../../src_/lib/foo/foo.ml.html" class="source_link">Source</a>
     </h1>

TODO: The page for the package 'foo' can't link to the page for the library 'foo'.

  $ grep href html/foo/index.html
   <head><title>foo (foo)</title><link rel="stylesheet" href="../odoc.css"/>
      <a href="#page-for-the-package" class="anchor"></a>Page for the package
     </h1><p>Library <a href="#"><code>foo</code></a>.</p>
