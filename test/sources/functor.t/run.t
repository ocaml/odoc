Verify the behavior on functors.

  $ odoc compile -c module-a -c src-source root.mld

  $ printf "s.ml\na.ml\nb.ml\n" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o src-source.odoc source_tree.map

  $ ocamlc -c -o s.cmo s.ml -bin-annot -I .
  $ ocamlc -c -o a.cmo a.ml -bin-annot -I .
  $ ocamlc -c -o b.cmo b.ml -bin-annot -I .
  $ odoc compile --source-name s.ml --source-parent-file src-source.odoc -I . s.cmt
  $ odoc compile --source-name a.ml --source-parent-file src-source.odoc -I . a.cmt
  $ odoc compile --source-name b.ml --source-parent-file src-source.odoc -I . b.cmt
  $ odoc link -I . s.odoc
  $ odoc link -I . a.odoc
  $ odoc link -I . b.odoc
  $ odoc html-generate --source s.ml --indent -o html s.odocl
  $ odoc html-generate --source a.ml --indent -o html a.odocl
  $ odoc html-generate --source b.ml --indent -o html b.odocl

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
  html/root
  html/root/source
  html/root/source/a.ml.html
  html/root/source/b.ml.html
  html/root/source/s.ml.html

In this test, the functor expansion contains the right link.

  $ cat html/A/F/index.html | grep source_link -C 1
     <h1>Module <code><span>A.F</span></code>
      <a href="../../root/source/a.ml.html#module-F" class="source_link">Source
      </a>
  --
       <a href="../../root/source/a.ml.html#module-F.type-t"
        class="source_link">Source
       </a>
  --
       <a href="#val-y" class="anchor"></a>
       <a href="../../root/source/a.ml.html#module-F.val-y" class="source_link">
        Source

  $ cat html/root/source/a.ml.html | grep L3
  <a id="L3" class="source_line" href="#L3">3</a>

However, on functor results, there is a link to source in the file:

  $ cat html/B/R/index.html | grep source_link -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>B.R</span></code>
      <a href="../../root/source/b.ml.html#module-R" class="source_link">Source
      </a>
     </h1>
  --
       <a href="#type-t" class="anchor"></a>
       <a href="../../root/source/a.ml.html#module-F.type-t"
        class="source_link">Source
       </a>
       <code><span><span class="keyword">type</span> t</span>
  --
      <div class="spec value anchored" id="val-y">
       <a href="#val-y" class="anchor"></a>
       <a href="../../root/source/a.ml.html#module-F.val-y" class="source_link">
        Source
       </a>

Source links in functor parameters might not make sense. Currently we generate none:

  $ cat html/A/F/argument-1-S/index.html | grep source_link -C 1
  [1]
