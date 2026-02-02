Verify the behavior on functors.

  $ ocamlc -c -o s.cmo s.ml -bin-annot -I .
  $ ocamlc -c -o a.cmo a.ml -bin-annot -I .
  $ ocamlc -c -o b.cmo b.ml -bin-annot -I .
  $ odoc compile-impl --source-id src/s.ml -I . s.cmt --output-dir .
  $ odoc compile -I . s.cmt
  $ odoc compile-impl --source-id src/a.ml -I . a.cmt --output-dir .
  $ odoc compile -I . a.cmt
  $ odoc compile-impl --source-id src/b.ml -I . b.cmt --output-dir .
  $ odoc compile -I . b.cmt
  $ odoc link -I . s.odoc
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc link -I . impl-s.odoc
  $ odoc link -I . impl-a.odoc
  $ odoc link -I . impl-b.odoc
  $ odoc html-generate-source --impl impl-s.odocl --indent -o html s.ml
  $ odoc html-generate --indent -o html s.odocl
  $ odoc html-generate-source --impl impl-a.odocl --indent -o html a.ml
  $ odoc html-generate --indent -o html a.odocl
  $ odoc html-generate-source --impl impl-b.odocl --indent -o html b.ml
  $ odoc html-generate --indent -o html b.odocl

  $ find html | sort
  html
  html/A
  html/A/F
  html/A/F/argument-1-S
  html/A/F/argument-1-S/index.html
  html/A/F/index.html
  html/A/index.html
  html/B
  html/B/R
  html/B/R/index.html
  html/B/S
  html/B/S/index.html
  html/B/index.html
  html/S
  html/S/index.html
  html/S/module-type-S
  html/S/module-type-S/index.html
  html/src
  html/src/a.ml.html
  html/src/b.ml.html
  html/src/s.ml.html

In this test, the functor expansion contains the right link.

  $ cat html/A/F/index.html | grep source_link -C 1
     <h1>Module <code><span>A.F</span></code>
      <a href="../../src/a.ml.html#module-F" class="source_link">Source</a>
     </h1>
  --
       <a href="#type-t" class="anchor"></a>
       <a href="../../src/a.ml.html#module-F.type-t" class="source_link">Source
       </a>
  --
       <a href="#val-y" class="anchor"></a>
       <a href="../../src/a.ml.html#module-F.val-y" class="source_link">Source
       </a>

  $ cat html/src/a.ml.html | grep L3
  <a id="L3" class="source_line" href="#L3">3</a>

However, on functor results, there is a link to source in the file:

  $ cat html/B/R/index.html | grep source_link -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>B.R</span></code>
      <a href="../../src/b.ml.html#module-R" class="source_link">Source</a>
     </h1>
    </header>
  --
      <div class="spec type anchored" id="type-t">
       <a href="#type-t" class="anchor"></a>
       <a href="../../src/a.ml.html#module-F.type-t" class="source_link">Source
       </a>
       <code><span><span class="keyword">type</span> t</span>
  --
      <div class="spec value anchored" id="val-y">
       <a href="#val-y" class="anchor"></a>
       <a href="../../src/a.ml.html#module-F.val-y" class="source_link">Source
       </a>
       <code>

Source links in functor parameters might not make sense. Currently we generate none:

  $ cat html/A/F/argument-1-S/index.html | grep source_link -C 1
  [1]
