Checking that source links exists inside recursive modules.

  $ odoc compile -c module-main -c srctree-source root.mld

  $ printf "main.ml" > source_tree.map
  $ odoc source-tree -I . --parent page-root -o srctree-source.odoc source_tree.map

  $ ocamlc -c main.ml -bin-annot -I .
  $ odoc compile-src --source-path main.ml --source-parent-file srctree-source.odoc -I . main.cmt
  $ odoc compile -I . main.cmt
  $ odoc link -I . src-main.odoc
  $ odoc link -I . main.odoc
  $ odoc html-generate --indent -o html main.odocl
  $ odoc html-generate --source main.ml --indent -o html src-main.odocl

Both modules should contain source links

  $ grep source_link html/Main/A/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.A</span></code>
      <a href="../../root/source/main.ml.html#module-A" class="source_link">
       Source
      </a>

  $ grep source_link html/Main/B/index.html -C 2
    <header class="odoc-preamble">
     <h1>Module <code><span>Main.B</span></code>
      <a href="../../root/source/main.ml.html#module-B" class="source_link">
       Source
      </a>
